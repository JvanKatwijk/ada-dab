/* Generic Viterbi decoder,
 * Copyright Phil Karn, KA9Q, 
 * Code has been slightly modified for use with Spiral (www.spiral.net)
 * Karn's original code can be found here: http://www.ka9q.net/code/fec/
 * May be used under the terms of the GNU Lesser General Public License (LGPL)
 * see http://www.gnu.org/copyleft/lgpl.html
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <memory.h>
#include <sys/resource.h>
#include "spiral.h"
//#include "viterbi_support.h"
#include <pmmintrin.h>
#include <emmintrin.h>
#include <xmmintrin.h>
#include <mmintrin.h>


static uint8_t Partab [] = 
{ 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
  1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
  1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
  0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
  1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
  0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
  0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
  1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
  1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
  0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
  0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
  1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
  0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
  1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
  1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
  0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0};

//
//	One could create the table above, i.e. a 256 entry
//	odd-parity lookup table by the following function
//	It is now precomputed, so the function here is not used.
void	partab_init (void){
int16_t i,cnt,ti;

	for (i = 0; i < 256; i++){
	   cnt = 0;
	   ti = i;
	   while (ti != 0) {
	      if (ti & 1) cnt++;
	      ti >>= 1;
	   }
	   Partab [i] = cnt & 1;
	}
}

int 	parity (int x){
	/* Fold down to one byte */
	x ^= (x >> 16);
	x ^= (x >> 8);
	return Partab [x];
}

//	returns NULL on success
extern int posix_memalign(void **memptr, size_t alignment, size_t size);

//decision_t is a BIT vector
typedef union {
	uint32_t t [NUMSTATES / 32];
	uint32_t w [NUMSTATES / 32];
	uint16_t s [NUMSTATES / 16];
	uint8_t  c [NUMSTATES / 8];
} decision_t __attribute__ ((aligned (16)));

typedef union {
	uint32_t t[NUMSTATES];
} metric_t __attribute__ ((aligned (16)));

void renormalize (uint32_t* X, uint32_t threshold){
int i;

	if (X [0] > threshold) {
	   uint32_t min = X [0];
	   for (i = 0; i < NUMSTATES; i++)
	      if (min > X[i])
	         min = X [i];
	         for (i = 0; i < NUMSTATES; i++)
	            X[i] -= min;
	}
}

uint32_t Branchtab [NUMSTATES / 2 * RATE] __attribute__ ((aligned (16)));

//	State info for instance of Viterbi decoder
struct v {
	__attribute__ ((aligned (16))) metric_t metrics1; //path metric buffer 1
	__attribute__ ((aligned (16))) metric_t metrics2; //path metric buffer 2
//	Pointers to path metrics, swapped on every bit 
	metric_t	*old_metrics;
	metric_t	*new_metrics; 
	decision_t	*decisions;   	//  decisions 
};

//	Initialize Viterbi decoder for start of new frame
int init_viterbi (void *p, int starting_state) {
struct v *vp = p;
int i;
	if (p == NULL)
	   return -1;

	for (i = 0; i < NUMSTATES; i++)
	   vp -> metrics1. t[i] = 63;

	vp -> old_metrics = &vp -> metrics1;
	vp -> new_metrics = &vp -> metrics2;
//	Bias known start state 
	vp -> old_metrics -> t[starting_state & (NUMSTATES-1)] = 0;
	return 0;
}

//	Create a new instance of a Viterbi decoder 
void	*create_viterbi (int len) {
void	*p;
struct v *vp;
static int Init = 0;

	if (!Init){
	   int state, i;
	   int polys [RATE] = POLYS;
	   for (state = 0;state < NUMSTATES / 2; state++) {
	      for (i = 0; i < RATE; i++) {
	         Branchtab [i * NUMSTATES / 2 + state] =
	               (polys [i] < 0) ^ parity((2 * state) & abs(polys[i])) ?
	                                        255 : 0;
	      }
	   }
	   Init++;
	}

	if (posix_memalign ((void**)&p, 16, sizeof (struct v)) != NULL)
	   return NULL;

	vp = (struct v *)p;
	if (posix_memalign ((void**)&vp -> decisions,
	                    16,
	                    (len + (K - 1)) * sizeof (decision_t)) != NULL){
	   free (vp);
	   return NULL;
	}

	init_viterbi (vp, 0);
	return vp;
}

//	Viterbi chainback 
int	chainback_viterbi (void *p,
	                   uint8_t  *data,     /* Decoded output data */
	                   uint32_t nbits,     /* Number of data bits */
	                   uint32_t endstate) { /* Terminal encoder state */
struct v *vp = p;
decision_t *d;

//	ADDSHIFT and SUBSHIFT make sure that the thing returned is a byte. 
//	for us, K == 7, so we can expand ourselves
#define	ADDSHIFT 2
#define	SUBSHIFT 0

//#if (K-1<8)
//#define ADDSHIFT (8-(K-1))
//#define SUBSHIFT 0
//#elif (K-1>8)
//#define ADDSHIFT 0
//#define SUBSHIFT ((K-1)-8)
//#else
//#define ADDSHIFT 0
//#define SUBSHIFT 0
//#endif

	if (p == NULL)
	   return -1;

	d = vp -> decisions;
//	Make room beyond the end of the encoder register so we can
//	accumulate a full byte of decoded data

	endstate = (endstate % NUMSTATES) << ADDSHIFT;
//	The store into data[] only needs to be done every 8 bits.
//	But this avoids a conditional branch, and the writes will
//	combine in the cache anyway

	d += (K - 1); 	// Look past tail 
	while (nbits-- != 0){
	   int k;
	   k = (d [nbits]. w [(endstate >> ADDSHIFT) / 32] >>
	                                 ((endstate >> ADDSHIFT) % 32)) & 1;
	   endstate = (endstate >> 1) | (k << (K - 2 + ADDSHIFT));
//	we know that SUBSHIFT == 0, so
//	   data [nbits >> 3] = endstate >> SUBSHIFT;
	   data [nbits >> 3] = endstate;
	}
	return 0;
}

//	Delete instance of a Viterbi decoder 
void	delete_viterbi (void *p) {
struct v *vp = p;

	if (vp != NULL) {
	   free (vp -> decisions);
	   free (vp);
	}
}

/* C-language butterfly */
void	BFLY (int i,
	      int s,
	      uint32_t * syms,
	      struct v * vp,
	      decision_t * d) {
int j, decision0, decision1;
uint32_t metric, m0, m1, m2, m3;

	metric = 0;
	for (j = 0; j < RATE; j++)
	   metric += (Branchtab [i + j * NUMSTATES / 2] ^
	                         syms [s * RATE + j]) >> METRICSHIFT;
	metric = metric >> PRECISIONSHIFT;
  
	const
	uint32_t max = ((RATE * ((256 - 1) >> METRICSHIFT)) >> PRECISIONSHIFT);
	m0 = vp -> old_metrics -> t [i] + metric;
	m1 = vp -> old_metrics -> t [i + NUMSTATES / 2] + (max - metric);
	m2 = vp -> old_metrics -> t [i] + (max - metric);
	m3 = vp -> old_metrics -> t [i + NUMSTATES / 2] + metric;
  
	decision0 = (signed int)(m0 - m1) > 0;
	decision1 = (signed int)(m2 - m3) > 0;
  
	vp -> new_metrics -> t [2 * i    ] = decision0 ? m1 : m0;
	vp -> new_metrics -> t [2 * i + 1] = decision1 ? m3 : m2;
  
	d -> w [i / (sizeof(unsigned int) * 8 / 2)+s*(sizeof(decision_t)/sizeof(unsigned int))] |= 
	        (decision0|decision1<<1) << ((2*i)&(sizeof(unsigned int)*8-1));
}


//	Update decoder with a block of demodulated symbols
//	Note that nbits is the number of decoded data bits, not the number
//	of symbols!

int	update_viterbi_blk_GENERIC (void *p,
	                            uint32_t *syms,
	                            int nbits) {
struct v *vp = p;
decision_t *d;
int s,i;

	if (p == NULL)
	   return -1;

	d = (decision_t *)vp -> decisions;
	for (s = 0; s < nbits; s++)
	   memset (d + s, 0, sizeof (decision_t));
	for (s = 0; s < nbits; s++){
	   void *tmp;
	   for (i = 0; i < NUMSTATES / 2; i++) {
	      BFLY (i, s, syms, vp, vp -> decisions);
	   }

	   renormalize (vp -> new_metrics -> t, RENORMALIZE_THRESHOLD);
///     Swap pointers to old and new metrics
	   tmp = vp -> old_metrics;
	   vp -> old_metrics = vp -> new_metrics;
	   vp -> new_metrics = tmp;
	}
	return 0;
}

