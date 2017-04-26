#define KIVEC(A) int A##n, const int*A##p
#define IVEC(A) int A##n, int*A##p

#define OK return 0;

#define STEP_IMP         \
    int k;               \
    for(k=0;k<xn;k++) {  \
        yp[k]=xp[k]>0;   \
    }                    \
    OK

int stepI(KIVEC(x),IVEC(y)) {
    STEP_IMP
}
