/* Copyright 1990-2008, Jsoftware Inc.  All rights reserved.               */
/* Licensed use only. Any other use is in violation of copyright.          */
/*                                                                         */
/* Parsing; see APL Dictionary, pp. 12-13 & 38.                            */

#include "j.h"
#include "p.h"


/* NVR - named value reference                                          */
/* a value referenced in the parser which is the value of a name        */
/* (that is, in some symbol table).                                     */
/*                                                                      */
/* jt->nvra      NVR stack a: stack of A values                         */
/* jt->nvrav     AAV(jt->nvra)                                          */
/* jt->nvrb      NVR stack b: corresponding to stack a --               */
/*               1 if unchanged; 0 if redefined                         */
/* jt->nvrbv     BAV(jt->nvrb)                                          */
/* jt->nvrtop    index of top of stack                                  */
/*                                                                      */
/* Each call of the parser records the current NVR stack top (nvrtop),  */
/* (nvrtop), and pop stuff off the stack back to that top on exit       */
/*                                                                      */
/* nvrpush(w):   w is a named value just moved from the parser queue    */
/*               to the parser stack.  Push w onto the NVR stack.       */
/* nvrpop(otop): pop stuff off the NVR stack back to the old top otop   */
/* nvrredef(w):  w is the value of a name about to be redefined         */
/*               (reassigned or erased).  checks whether w is in the    */
/*               NVR stack                                              */


B jtparseinit(J jt){A x;
 GA(x,INT,20,1,0); ra(x); jt->nvra=x; jt->nvrav=AAV(x);
 GA(x,B01,20,1,0); ra(x); jt->nvrb=x; jt->nvrbv=BAV(x);
 R 1;
}

static F1(jtnvrpush){
 if(jt->nvrtop==AN(jt->nvra)){
  RZ(jt->nvra=ext(1,jt->nvra)); jt->nvrav=AAV(jt->nvra);
  while(AN(jt->nvrb)<AN(jt->nvra))RZ(jt->nvrb=ext(1,jt->nvrb)); jt->nvrbv=BAV(jt->nvrb);
 }
 jt->nvrav[jt->nvrtop]=w;
 jt->nvrbv[jt->nvrtop]=1;
 ++jt->nvrtop;
 R w;
}

static void jtnvrpop(J jt,I otop){A*v=otop+jt->nvrav;B*b=otop+jt->nvrbv;
 DO(jt->nvrtop-otop, if(!*b++)fa(*v); ++v;);
 jt->nvrtop=otop;
}

void jtnvrredef(J jt,A w){A*v=jt->nvrav;B*b=jt->nvrbv;
 DO(jt->nvrtop, if(w==*v++){if(b[i]){ra(w); b[i]=0;} break;});
}    /* stack handling for w which is about to be redefined */

// Action routines for the parse, when an executable fragment has been detected.  Each routine must:
// set jt->sitop->dci to the word number to flag in case the action fails
// collect the arguments for the action and run it
// Return failure if the action failed
// Save the result in the last stack entry for the fragment
// Set the word-number entry in the last stack entry to the word number that the result will go by
// Close up any gap between the unexecuted stack elements and the result
// Return the new stack pointer, which is the relocated beginning-of-stack
ACTION(jtmonad1 ){jt->sitop->dci=(I)(stack[5] = stack[3]); RZ (stack[4] = dfs1(stack[4],stack[2])); stack[3] = stack[1]; stack[2] = stack[0]; R stack+2;}
ACTION(jtmonad2 ){jt->sitop->dci=(I)(stack[7] = stack[5]); RZ (stack[6] = dfs1(stack[6],stack[4])); stack[4] = stack[2]; stack[2] = stack[0]; stack[5] = stack[3]; stack[3] = stack[1]; R stack+2;}
ACTION(jtdyad   ){jt->sitop->dci=(I)(stack[7] = stack[5]); RZ(stack[6] = dfs2(stack[2], stack[6], stack[4])); stack[5] = stack[1]; stack[4] = stack[0]; R stack + 4; }
ACTION(jtadv){ jt->sitop->dci = (I)stack[5]; RZ(stack[4] = dfs1(stack[2], stack[4])); stack[5] = stack[3]; stack[3] = stack[1]; stack[2] = stack[0]; R stack + 2; }
ACTION(jtconj){ jt->sitop->dci = (I)stack[5]; RZ(stack[6] = dfs2(stack[2], stack[6], stack[4])); stack[7] = stack[3]; stack[5] = stack[1]; stack[4] = stack[0]; R stack + 4; }
ACTION(jttrident){jt->sitop->dci=(I)(stack[7] = stack[3]); RZ(stack[6] = folk(stack[2], stack[4], stack[6])); stack[5] = stack[1]; stack[4] = stack[0]; R stack + 4; }
ACTION(jtbident ){jt->sitop->dci=(I)(stack[5] = stack[3]); RZ(stack[4] = hook(stack[2], stack[4])); stack[3] = stack[1]; stack[2] = stack[0]; R stack + 2; }
ACTION(jtpunc   ){stack[5] = stack[1]; stack[4] = stack[2]; R stack+4;}  // Can't fail; use token # from (

#if 0  // this has been inlined into parsea
static ACTION(jtmove){A z;
 z=stack[MAX(0,e)];
 if(!(NAME&AT(z))||ASGN&AT(stack[b]))R z;
 RZ(z=jt->xdefn&&NMDOT&NAV(z)->flag?symbrd(z):nameref(z));
 R nvrpush(z);
}
#endif

static F2(jtisf){R symbis(onm(a),CALL1(jt->pre,w,0L),jt->symb);} 

ACTION(jtis){A f,n,v;B ger=0;C c,*s;
 jt->sitop->dci=(I)(stack[5]=stack[3]);n=stack[0]; v=stack[4];   // action-routine housekeeping
 if((I)stack[1]==1)jt->asgn = 1;  // if the word number of the lhs is 1, it's either (noun)=: or name=: or 'value'=: at the beginning of the line; so indicate
 if(LIT&AT(n)&&1>=AR(n)){
  ASSERT(1>=AR(n),EVRANK); 
  s=CAV(n); ger=CGRAVE==*s; 
  RZ(n=words(ger?str(AN(n)-1,1+s):n));
  if(1==AN(n))RZ(n=head(n));
 }
 ASSERT(AN(n)||!IC(v),EVILNAME);
 f=stack[2]; c=*CAV(f); jt->symb=jt->local&&c==CASGN?jt->local:jt->global;
 if(NAME&AT(n)) symbis(n,v,jt->symb);
 else if(!AR(n))symbis(onm(n),v,jt->symb);
 else {ASSERT(1==AR(n),EVRANK); jt->pre=ger?jtfxx:jtope; rank2ex(n,v,0L,-1L,-1L,jtisf);}
 jt->symb=0;
 RNE(stack+4);  // the result is the same value that was assigned
}


#define AVN   (     ADV+VERB+NOUN)
#define CAVN  (CONJ+ADV+VERB+NOUN)
#define EDGE  (MARK+ASGN+LPAR)

ACTF casefuncs[] = { jtmonad1, jtmonad2, jtdyad, jtadv, jtconj, jttrident, jtbident, jtis, jtpunc};
PT cases[] = {
 EDGE,      VERB,      NOUN, ANY,       jtmonad1,  jtvmonad, 1,2,1,
 EDGE+AVN,  VERB,      VERB, NOUN,      jtmonad2,  jtvmonad, 2,3,2,
 EDGE+AVN,  NOUN,      VERB, NOUN,      jtdyad,    jtvdyad,  1,3,2,
 EDGE+AVN,  VERB+NOUN, ADV,  ANY,       jtadv,     jtvadv,   1,2,1,
 EDGE+AVN,  VERB+NOUN, CONJ, VERB+NOUN, jtconj,    jtvconj,  1,3,1,
 EDGE+AVN,  VERB+NOUN, VERB, VERB,      jttrident, jtvfolk,  1,3,1,
 EDGE,      CAVN,      CAVN, ANY,       jtbident,  jtvhook,  1,2,1,
 NAME+NOUN, ASGN,      CAVN, ANY,       jtis,      jtvis,    0,2,1,
 LPAR,      CAVN,      RPAR, ANY,       jtpunc,    jtvpunc,  0,2,0,
};
#if 0 // this is the formal search per the Dictionary
I *c;
for (i = 0; i<NCASES; i++){
    c = cases[i].c; s = n + stack;
    if (*c++&AT(*s++) && *c++&AT(*s++) && *c++&AT(*s++) && *c++&AT(*s++)) break;
}
#endif

F1(jtparse){A z;
 RZ(w);
 RZ(deba(DCPARSE,0L,w,0L));
 z=parsea(w);
 debz();
 R z;
}

// Parse a J sentence.  Input is the queue of tokens
F1(jtparsea){A *stack,*queue,y,z;I es,i,m,otop=jt->nvrtop;                  
 RZ(w);
 // This routine has two global responsibilities in addition to parsing.  jt->asgn must be set to 1
 // if the last thing is an assignment, and since this flag is cleared during execution (by ". and
 // others), it must be set at the time the assignment is executed.  We catch it in the action routine,
 // noting when the assignment is to (possibly inherited) word 1 (word 0 is the mark).
 //
 // jt->sitop->dci must be set before executing anything that might fail; it holds the original
 // word number+1 of the token that failed.  jt->sitop->dci is set by the individual action routines,
 // or here only at the end of processing.  It's a pity that this needs to be set BEFORE executing the
 // action routine - otherwise we could set it only on failure - but malloc seems to longjmp() in some
 // error cases, so we can't be sure that all function calls will return.  Could that be changed?
 m=AN(w); jt->asgn = 0; ++jt->parsercalls;
 if(1>m)R mark;
 // to simulate the mark at the head of the queue, we set queue to point to the -1 position which
 // is an out-of-bounds entry that must never be referenced.  m=0 corresponds to this mark; otherwise queue[m] is original
 // word m-1, with the number in the stack being one higher than the original word number
 queue=AAV(w)-1; 
 // allocate the stack.  No need to initialize it, except for the marks at the end, because we
 // never look at a stack location until we have moved from the queue to that position.
 // Each word gets two stack locations: first is the word itself, second the original word number+1
 // to use if there is an error on the word
 GA(y,BOX,2*(m+4),1,0); stack=(2*(m+1))+AAV(y);  // 4 marks: 1 at beginning, 3 at end
 stack[0] = stack[2] = stack[4] = mark;  // install initial marks.  word numbers are immaterial
 es = MIN(2,m-1); // number of extra words to pull from the queue.  We always need 2 words afetr the first before a match is possible.
   // but number of pulls, which is 1+es, must never exceed m
 while(1){  // till no more matches possible...
#define ST(i) AT(stack[2*i])
  if(ST(2) & CAVN) { // cases 0-7
   if(ST(0) == NAME)i = 7;   // NAME is set only if followed by ASGN
   else {  // cases 0-6
    if (!(ST(0)&(EDGE + AVN)))i = NCASES;
    else if ((ST(1) | ST(2))&(ADV + CONJ)){ /* cases 3, 4, 6, but only if ST(1) is CAVN  */
     i = ST(1)&(ADV + CONJ)
         ? (ST(0)&EDGE ? 6 : NCASES)
         : ST(1)&CAVN
           ? (ST(2)==ADV ? 3 : ST(3)&(VERB + NOUN) ? 4 : ST(0)&EDGE ? 6 : NCASES)
           : NCASES;
    } else { /* cases 0, 1, 2, 5, 6, , but only if ST(1) is CAVN.  ST(1) is NOUN, VERB, or other nommatching such as (  */
     if (ST(2)&NOUN){
         i = ST(0)&EDGE ? (ST(1) == VERB ? 0 : ST(1)&NOUN ? 6 : NCASES) : (ST(1) == ASGN && ST(0)&NOUN) ? 7 : NCASES;
     } else {
      i = ST(3)&NOUN
          ? (ST(1)&NOUN ? 2 : ST(1) == VERB ? 1 : (ST(1) == ASGN && ST(0)&NOUN) ? 7 : NCASES)
          : ST(1)&CAVN ? (ST(3) == VERB ? 5 : ST(0)&EDGE ? 6 : NCASES) : (ST(1) == ASGN && ST(0)&NOUN) ? 7 : NCASES;
     }
    }
   }
  } else i = (ST(0)==LPAR && ST(1)&CAVN && ST(2)==RPAR) ? 8 : NCASES;  // case 8.  Test LPAR first, because if LPAR doesn't match, the
                   // sentence will have a syntax error, while RPAR might just be passing through
#undef ST

  if(i<NCASES){
   // This is where we execute the action routine.  We give it the stack frame; it is responsible
   // for finding its arguments on the stack, storing the result (if no error) over the last
   // stack entry, then closing up any gap between the front-of-stack and the executed fragment, 
   // and finally returning the new front-of-stack pointer 
   if(!(stack = casefuncs[i](jt,stack)))break;  // stop parsing in case of error
  }else{
   // no executable fragment, pull from the stack.  If we pull ')', there is no way we can execute
   // anything till 2 more words have been pulled, so we pull them here to avoid parse overhead.
   // Likewise, if we pull a CONJ, we can safely pull 1 more here.  And first time through, we should
   // pull 2 words following the first one.
   // es holds the number of extra pulls required.  It may go negative, which means no extra pulls.
   // the only time it is positive coming into the loop below is the initial filling of the queue
   if(m<=0) {  // Toward the end we have to worry about underrunning the queue, and pulling the virtual mark
     if(m==0) {*--stack = 0; *--stack = mark; --m; continue;}  // unstructured branch!! move in the mark and use it
     else { jt->sitop->dci = 0; break;}   // if there's nothing more to pull, stop big loop
   }
   do{
    I at, oldm = m;  // type of the new word (before name resolution); m on input (word # of next word)
    stack -= 2;  // back up to new stack frame, where we will store the new word

    // Move in the new word and check its type.  If it is a name that is not being assigned, resolve its
    // value.
    at=AT(y = queue[m--]);   // known to be nonzero
    if(at==NAME) {
     stack[1] = (A)oldm;  // set original word number+1 for the word moved
     if(AT(stack[2])!=ASGN) {  // Replace a name with its value, unless to left of ASGN
      jt->sitop->dci = oldm;   // In case name resolution fails, we'd better have an error word number
      if (!(y = jt->xdefn&&NMDOT&NAV(y)->flag ? symbrd(y) : nameref(y))) { stack = 0; goto exitparse; }
      nvrpush(y);
     }
    // If the new word was not a name (whether assigned or not), look to see if it is ) or a conjunction,
    // which allow 2 or 1 more pulls from the queue without checking for a fragment.
    // NOTE that we are using the original type for the word, which will be obsolete if the word was a
    // name that was replaced by name resolution.  We don't care - RPAR was never a name to begin with, and CONJ
    // is much more likely to be a primitive; and we don't want to take the time to refetch the resolved type
    } else if(at&RPAR+CONJ){
     if(at==RPAR){es = MIN(m,2);}  //  Note we don't set stack[1] for RPAR.  It would be OK to, but we're testing for RPAR anyway, and the word number of RPAR will never be used
     else{stack[1] = (A)oldm; es = MIN(m,1);}
    } else{stack[1] = (A)oldm;}   // For all else, just install the word number 
    // y has the resolved value, which is never a NAME unless there is an assignment
    stack[0] = y;   // finish setting the stack entry, with the new word
   }while(es-->0);  // Repeat if more pulls required.  We also exit with stack==0 if there is an error
   // DO NOT PUT ANYTHING HERE! the continue above will skip it
  }
 }  // break with stack==0 on error; main exit is when queue is empty (m<0)
exitparse:
 nvrpop(otop);
 RZ(stack);  // If there was an error during execution or name-stacking, exit with failure
 z=stack[2];   // stack[0..1] are the mark; this is the sentence result, if there is no error
 ASSERT(AT(z)&MARK+CAVN&&AT(stack[4])==MARK,EVSYNTAX);  // OK if 0 or 1 words left (0 should not occur)
 R z;
}

/* locals in parsea                                             */
/* i:     index in cases[] of matching 4-pattern                */
/* m:     current # of non-mark words in the queue              */
/* otop:  old value of jt->nvrtop                               */
/* queue: queue, taken from the input                           */
/* stack: parser stack, (word,count) pairs                      */
/* w:     argument containing the queue                         */
/* y:     array temp                                            */
/* z:     result                                                */
