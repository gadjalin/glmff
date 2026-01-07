#if defined(__GFORTRAN__) || defined(NAGFOR)
#  define PASTE(a) a
#  define CAT(a,b) PASTE(a)b
#  define CAT3(a,b,c) CAT(a,b)c
#  define CAT4(a,b,c,d) CAT3(a,b,c)d
#else
#  define PCAT(a,b,c,d) a ## b ## c ## d
#  define CAT(a,b) PCAT(a,b,,)
#  define CAT3(a,b,c) PCAT(a,b,c,)
#  define CAT4(a,b,c,d) PCAT(a,b,c,d)
#endif

#define GLMFF_CLIP_CONTROL_LH_ZO 1
#define GLMFF_CLIP_CONTROL_RH_ZO 2
#define GLMFF_CLIP_CONTROL_LH_NO 3
#define GLMFF_CLIP_CONTROL_RH_NO 4

#ifdef GLMFF_FORCE_DEPTH_ZERO_TO_ONE
#  ifdef GLMFF_FORCE_LEFT_HANDED
#    define GLMFF_CONFIG_CLIP_CONTROL GLMFF_CLIP_CONTROL_LH_ZO
#  else
#    define GLMFF_CONFIG_CLIP_CONTROL GLMFF_CLIP_CONTROL_RH_ZO
#  endif
#else
#  ifdef GLMFF_FORCE_LEFT_HANDED
#    define GLMFF_CONFIG_CLIP_CONTROL GLMFF_CLIP_CONTROL_LH_NO
#  else
#    define GLMFF_CONFIG_CLIP_CONTROL GLMFF_CLIP_CONTROL_RH_NO
#  endif
#endif

