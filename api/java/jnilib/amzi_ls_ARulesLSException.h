/* DO NOT EDIT THIS FILE - it is machine generated */
#include <jni.h>
/* Header for class amzi_ls_ARulesLSException */

#ifndef _Included_amzi_ls_ARulesLSException
#define _Included_amzi_ls_ARulesLSException
#ifdef __cplusplus
extern "C" {
#endif
#undef amzi_ls_ARulesLSException_serialVersionUID
#define amzi_ls_ARulesLSException_serialVersionUID -3042686055658047285i64
#undef amzi_ls_ARulesLSException_serialVersionUID
#define amzi_ls_ARulesLSException_serialVersionUID -3387516993124229948i64
#undef amzi_ls_ARulesLSException_BADENG
#define amzi_ls_ARulesLSException_BADENG 0L
#undef amzi_ls_ARulesLSException_ABORT
#define amzi_ls_ARulesLSException_ABORT 1L
#undef amzi_ls_ARulesLSException_INTERNAL
#define amzi_ls_ARulesLSException_INTERNAL 2L
#undef amzi_ls_ARulesLSException_FATAL
#define amzi_ls_ARulesLSException_FATAL 3L
#undef amzi_ls_ARulesLSException_INIT
#define amzi_ls_ARulesLSException_INIT 4L
#undef amzi_ls_ARulesLSException_API
#define amzi_ls_ARulesLSException_API 5L
#undef amzi_ls_ARulesLSException_LOAD
#define amzi_ls_ARulesLSException_LOAD 6L
#undef amzi_ls_ARulesLSException_EXEC
#define amzi_ls_ARulesLSException_EXEC 7L
#undef amzi_ls_ARulesLSException_READ
#define amzi_ls_ARulesLSException_READ 8L
#undef amzi_ls_ARulesLSException_UNKNOWN
#define amzi_ls_ARulesLSException_UNKNOWN 9L
/*
 * Class:     amzi_ls_ARulesLSException
 * Method:    GetType
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_amzi_ls_ARulesLSException_GetType
  (JNIEnv *, jobject);

/*
 * Class:     amzi_ls_ARulesLSException
 * Method:    GetRC
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_amzi_ls_ARulesLSException_GetRC
  (JNIEnv *, jobject);

/*
 * Class:     amzi_ls_ARulesLSException
 * Method:    GetLineno
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_amzi_ls_ARulesLSException_GetLineno
  (JNIEnv *, jobject);

/*
 * Class:     amzi_ls_ARulesLSException
 * Method:    GetMsg
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_amzi_ls_ARulesLSException_GetMsg
  (JNIEnv *, jobject);

/*
 * Class:     amzi_ls_ARulesLSException
 * Method:    GetReadFileName
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_amzi_ls_ARulesLSException_GetReadFileName
  (JNIEnv *, jobject);

/*
 * Class:     amzi_ls_ARulesLSException
 * Method:    GetReadBuffer
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_amzi_ls_ARulesLSException_GetReadBuffer
  (JNIEnv *, jobject);

/*
 * Class:     amzi_ls_ARulesLSException
 * Method:    GetCallStack
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_amzi_ls_ARulesLSException_GetCallStack
  (JNIEnv *, jobject);

#ifdef __cplusplus
}
#endif
#endif
