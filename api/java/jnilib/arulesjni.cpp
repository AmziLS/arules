/* Amzi! interface DLL for Java 1.2

   Note - Both Java and Amzi! are Unicode based, so the string
   conversion routines used here are the jstring to Unicode ones,
   not the UTF-8 ones. To build this project, make sure the
   _UNICODE flag is set for the compiler. 

  $History: $

*/

#if defined(_WINDOWS) || defined(_WIN32)
#include <windows.h>
#endif
#include "amzi.h"
#include "amzi_ls_ARulesLogicServer.h"
#include "amzi_ls_ARulesLSException.h"
#include "arulesjni.h"

extern "C"
{
RC LSAPI RegisterRuntime(ENGid, unsigned long, char*, char*);
}

//#include <stdlib>

//#if defined(__sun)
//#include "stdlib.h"
//#include <string.h>
//#include <iostream.h>
//#endif

//#if defined(LINUX)
//#include <stdlib.h>
//#include <cstring>
//#endif

//#include <cstdlib>
#include <stdlib.h>
#include <cstring>

#ifdef MACOSX
#include <wchar.h>
#endif

#if defined(__unix__)
extern "C" { 
int aLinkW(void(*pfM)(aCHAR*), int argctr, aCHAR **pargv);
}
void disp_msg(aCHAR *);
#else
extern "C" int __cdecl aLinkW(void (__cdecl *pfM)(aCHAR*), int argctr, aCHAR **pargv);
void __cdecl disp_msg(aCHAR *);
#endif

void amzi_error(JNIEnv * jenv, ENGid curEng, char* lsapif);

#ifndef _MAX_PATH
const int _MAX_PATH = 512;
#endif

// Sun's JNI use unsigned short for Unicode characters,
// rather than wchar_t.  So we need both types to see if
// we can get the compiler happy about various arguments.
// Further, because we can't be sure wchar_t is a short,
// we need to have another conversion.  aaarghh.

#if defined(__sun) && defined(_UNICODE)
typedef unsigned short jCHAR;
#elif (defined(MACOSX) || defined(LINUX) || defined(HPUX)) && defined(_UNICODE)
typedef jchar jCHAR;
#else
typedef jchar jCHAR;
#endif

#define aS(x) L ## x

aCHAR MsgBuf[MAXMSG];  // A buffer for error messages
jCHAR jcMsgBuf[MAXMSG];

// The JNIEnv pointer is not valid from one thread to another.
// So to allow multiple threads, we have all of the exec/call
// functions set this global variable, which is then used
// in extended predicate execution.
JNIEnv *jenv_current;

void ac_to_jc(jCHAR *jc, aCHAR *ac, int len)
{
   for (int i=0; i<len; i++)
      jc[i] = ac[i];
   jc[len] = 0;
}

void a_to_jc(jCHAR *jc, char *ac, int len)
{
   for (int i=0; i<len; i++)
      jc[i] = ac[i];
   jc[len] = 0;
}

/* Get the Engine ID from the Java object. */

#ifdef __GNUC__
#define GET_EID(E)\
   ENGid    E;\
   jclass   jcls;\
   jfieldID jeid;\
   jcls = jenv->GetObjectClass(jobj);\
   jeid = jenv->GetFieldID(jcls, "curEng", "J");\
   E = (ENGid)(int)(jenv->GetLongField(jobj, jeid));
  // E = (ENGid)(jenv->GetIntField(jobj, jeid));
#else
#define GET_EID(E)\
   ENGid    E;\
   jclass   jcls;\
   jfieldID jeid;\
   jcls = jenv->GetObjectClass(jobj);\
   jeid = jenv->GetFieldID(jcls, "curEng", "J");\
   E = (ENGid)(jenv->GetLongField(jobj, jeid));
#endif

/*
ENGid _get_eid(JNIEnv * jenv, jobject jobj)
{
   ENGid    E;
   jclass   jcls;
   jfieldID jeid;
   jcls = jenv->GetObjectClass(jobj);
   jeid = jenv->GetFieldID(jcls, "curEng", "J");
   E = (ENGid)(jenv->GetIntField(jobj, jeid));
   return E;
}
*/

/* This function is used to make a clean copy of a Java string
   that can be used by the LSAPI interface.  It assumes that
   jenv is the pointer to the Java environment. You need to
   delete the string when it is no longer in use. */

aCHAR* JtoC(JNIEnv * jenv, jstring jstr)
{
   const jCHAR *jtmp;
   aCHAR* cstr;
   int    len;

   // Note that GetStringChars does not return
   // a null terminated Unicode string, but rather
   // just an array, that appears null terminated
   // for arrays with an odd length.
   jtmp = jenv->GetStringChars(jstr, NULL);
   if (jtmp == NULL)
   {
      jclass jerror = jenv->FindClass("java/lang/Error");
      jenv->ThrowNew(jerror, "Java string conversion error, possible bad LSAPI argument.");
   }

   len = jenv->GetStringLength(jstr);
   cstr = new aCHAR[len+1];
   for (int i=0; i < len; i++)
      cstr[i] = (aCHAR)jtmp[i];
   cstr[len] = 0;
   jenv->ReleaseStringChars(jstr, jtmp);

   return cstr;
}

/* Another version for non-Unicode strings */

char* JtoCA(JNIEnv * jenv, jstring jstr)
{
   const char *jtmp;
   char* cstr;
   int    len;

   // Note that GetStringChars does not return
   // a null terminated string, but rather
   // just an array.
   jtmp = jenv->GetStringUTFChars(jstr, NULL);
   if (jtmp == NULL)
   {
      jclass jerror = jenv->FindClass("java/lang/Error");
      jenv->ThrowNew(jerror, "Java string conversion error, possible bad LSAPI argument.");
   }
   len = jenv->GetStringUTFLength(jstr);
   cstr = new char[len+1];
   for (int i=0; i < len; i++)
      cstr[i] = (char)jtmp[i];
   cstr[len] = 0;
   jenv->ReleaseStringUTFChars(jstr, jtmp);

   return cstr;
}


JExtPred::~JExtPred()
{
   if (jobj != NULL) jenv_current->DeleteGlobalRef(jobj);
   delete[] jmeth_name;
   delete[] jclass_name;
}

JExtPred* EpList = NULL;

TF EXPFUNC p_java(void* p)
// AddPred will add as its predicate:
//		AddPred("predname", arity, &::p_java, ptr to the JExtPred block);
{
   jboolean jtf;
   JExtPred* ep = (JExtPred*)p;
   
   // don't use saved one
   //ep->jenv = jenv_current;

   if (ep->jobj == NULL)
   {
      if (ep->jcl == NULL)
         ep->jcl = ep->jenv->FindClass(ep->jclass_name);
      ep->jmeth = ep->jenv->GetStaticMethodID(ep->jcl, ep->jmeth_name, "()Z");
      if (ep->jmeth == NULL)
         return false;
      jtf = ep->jenv->CallStaticBooleanMethod(ep->jcl, ep->jmeth);
   }
   else
   {
      // strange, this used to work I thought, but comes up NULL on
      // second invocation, but ObjectClass works OK.  hmmmm.
      //ep->jcl = ep->jenv->FindClass(ep->jclass_name);
      ep->jcl = ep->jenv->GetObjectClass(ep->jobj);
      ep->jmeth = ep->jenv->GetMethodID(ep->jcl, ep->jmeth_name, "()Z");
      if (ep->jmeth == NULL)
         return false;
      jtf = ep->jenv->CallBooleanMethod(ep->jobj, ep->jmeth);
   }

   return (TF)jtf;
}


void amzi_error(JNIEnv * jenv, ENGid curEng, char* lsapif)
{
   char *msg;
   char preface[] = "Logic Server exception in ";
   jclass jclsxcpt;
   jfieldID jeid;
   jmethodID jconstruct;
   int len;
   aCHAR* wmsg;
   jCHAR* jcmsg;
   jstring jmsg;
   //jobject jolsxcpt;
   jthrowable jolsxcpt;
   
   //goto errorerror;

   // Get the package/class for Amzi! exceptions
   jclsxcpt = jenv->FindClass("amzi/ls/ARulesLSException");
   if (jclsxcpt == NULL) goto errorerror;

   // Get the constructor with one string argument
   jconstruct = jenv->GetMethodID(jclsxcpt, "<init>", "(Ljava/lang/String;)V");
   if (jconstruct == NULL) goto errorerror;

   // Create a message and initialize LSException object with it
   len = strlen(lsapif) + strlen(preface);
   msg = new char[len+1];
   strcpy(msg, preface);
   strcat(msg, lsapif);
   len = strlen(msg);
   wmsg = new aCHAR[len+1];
   mbstowcs(wmsg, msg, len+1);
   jcmsg = new jCHAR[len+1];
   ac_to_jc(jcmsg, wmsg, len);
   delete[] msg;
   delete[] wmsg;
   jmsg = jenv->NewString(jcmsg, len);
   delete[] jcmsg;
   jolsxcpt = (jthrowable)(jenv->NewObject(jclsxcpt, jconstruct, jmsg));
   if (jolsxcpt == NULL) goto errorerror;

   jeid = jenv->GetFieldID(jclsxcpt, "curEng", "J");
   jenv->SetLongField(jolsxcpt, jeid, (long)curEng);

   //jenv->Throw((_jthrowable*)jolsxcpt);
   jenv->Throw(jolsxcpt);
   return;

errorerror:
   jclass jerror = jenv->FindClass("java/lang/Error");
   jenv->ThrowNew(jerror, "Error processing ARules exception");
   return;
}

/* Implementation LSException */
/* -------------------------- */

/*
 * Class:     amzi_ls_ARulesLSException
 * Method:    GetType
 * Signature: ()I
 */
extern "C" JNIEXPORT jint JNICALL Java_amzi_ls_ARulesLSException_GetType
  (JNIEnv * jenv, jobject jobj)
{
   GET_EID(e);

   ExType type = lsGetExceptType(e);
   return (jint)type;
}

/*
 * Class:     amzi_ls_ARulesLSException
 * Method:    GetRC
 * Signature: ()I
 */
extern "C" JNIEXPORT jint JNICALL Java_amzi_ls_ARulesLSException_GetRC
  (JNIEnv * jenv, jobject jobj)
{
   GET_EID(e);

   RC rc = lsGetExceptRC(e);
   return (jint)rc;
}

/*
 * Class:     amzi_ls_ARulesLSException
 * Method:    GetLineno
 * Signature: ()I
 */
extern "C" JNIEXPORT jint JNICALL Java_amzi_ls_ARulesLSException_GetLineno
  (JNIEnv * jenv, jobject jobj)
{
   GET_EID(e);

   int lineno = lsGetExceptLineno(e);
   return (jint)lineno;
}
/*
 * Class:     amzi_ls_ARulesLSException
 * Method:    GetMsg
 * Signature: ()Ljava/lang/String;
 */
extern "C" JNIEXPORT jstring JNICALL Java_amzi_ls_ARulesLSException_GetMsg
  (JNIEnv * jenv, jobject jobj)
{
   GET_EID(e);
   lsGetExceptMsg(e, MsgBuf, MAXMSG);
   int len = wcslen(MsgBuf);
   ac_to_jc(jcMsgBuf, MsgBuf, len);
   jstring jstr = jenv->NewString(jcMsgBuf, len);

   return jstr;
}

/*
 * Class:     amzi_ls_ARulesLSException
 * Method:    GetReadFileName
 * Signature: ()Ljava/lang/String;
 */
extern "C" JNIEXPORT jstring JNICALL Java_amzi_ls_ARulesLSException_GetReadFileName
  (JNIEnv * jenv, jobject jobj)
{
   GET_EID(e);
   lsGetExceptReadFileName(e, MsgBuf, MAXMSG);
   int len = wcslen(MsgBuf);
   ac_to_jc(jcMsgBuf, MsgBuf, len);
   jstring jstr = jenv->NewString(jcMsgBuf, len);

   return jstr;
}

/*
 * Class:     amzi_ls_ARulesLSException
 * Method:    GetReadBuffer
 * Signature: ()Ljava/lang/String;
 */
extern "C" JNIEXPORT jstring JNICALL Java_amzi_ls_ARulesLSException_GetReadBuffer
  (JNIEnv * jenv, jobject jobj)
{
   GET_EID(e);
   lsGetExceptReadBuffer(e, MsgBuf, MAXMSG);
   int len = wcslen(MsgBuf);
   ac_to_jc(jcMsgBuf, MsgBuf, len);
   jstring jstr = jenv->NewString(jcMsgBuf, len);

   return jstr;
}

/*
 * Class:     amzi_ls_ARulesLSException
 * Method:    GetCallStack
 * Signature: ()Ljava/lang/String;
 */
extern "C" JNIEXPORT jstring JNICALL Java_amzi_ls_ARulesLSException_GetCallStack
  (JNIEnv * jenv, jobject jobj)
{
   GET_EID(e);
   lsGetExceptCallStack(e, MsgBuf, MAXMSG);
   int len = wcslen(MsgBuf);
   ac_to_jc(jcMsgBuf, MsgBuf, len);
   jstring jstr = jenv->NewString(jcMsgBuf, len);

   return jstr;
}


/* Implementation LogicServer */
/* -------------------------- */

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    Init
 * Signature: (Ljava/lang/String;)V
 */
extern "C" JNIEXPORT void JNICALL Java_amzi_ls_ARulesLogicServer_Init
  (JNIEnv * jenv, jobject jobj, jstring jstr)
{
   ENGid curEng;
   RC    rc;
   jclass   jcls;
   jfieldID jeid;
   aCHAR* ininame;
   
   EpList = NULL;   // Initialize extended predicates

   ininame = JtoC(jenv, jstr);

   rc = lsInit(&curEng, ininame);
   delete[] ininame;

   if (curEng == NULL)
   {
      jclass jerror = jenv->FindClass("java/lang/OutOfMemoryError");
      jenv->ThrowNew(jerror, "Unable to allocate new Logic Server");
   }

   if (rc != OK)
      amzi_error(jenv, curEng, "Init");

   jcls = jenv->GetObjectClass(jobj);
   jeid = jenv->GetFieldID(jcls, "curEng", "J");
   jenv->SetLongField(jobj, jeid, (int)curEng);

   return;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    Init2
 * Signature: (Ljava/lang/String;)V
 */
extern "C" JNIEXPORT void JNICALL Java_amzi_ls_ARulesLogicServer_Init2
  (JNIEnv * jenv, jobject jobj, jstring jstr)
{
   ENGid curEng;
   RC    rc;
   jclass   jcls;
   jfieldID jeid;
   aCHAR* inistr;

   EpList = NULL;    // Initialize extended predicates.
   
   inistr = JtoC(jenv, jstr);
   rc = lsInit2(&curEng, inistr);
   delete[] inistr;

   if (curEng == NULL)
   {
      jclass jerror = jenv->FindClass("java/lang/OutOfMemoryError");
      jenv->ThrowNew(jerror, "Unable to allocate new Logic Server");
   }

   if (rc != OK)
      amzi_error(jenv, curEng, "Init2");

   jcls = jenv->GetObjectClass(jobj);
   jeid = jenv->GetFieldID(jcls, "curEng", "J");
   jenv->SetLongField(jobj, jeid, (int)curEng);

   return;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    InitLSX
 * Signature: (J)V
 */
extern "C" JNIEXPORT void JNICALL Java_amzi_ls_ARulesLogicServer_InitLSX
  (JNIEnv * jenv, jobject jobj, jlong ptr)
{
   RC  rc;

   GET_EID(e);
   rc = lsInitLSX(e, (void*)(ajptr)ptr);
   if (rc != OK)
      amzi_error(jenv, e, "InitLSX");

   return;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    AddPred-8
 * Signature: (Ljava/lang/String;ILjava/lang/String;Ljava/lang/String;Ljava/lang/Object;Ljava/lang/Object;)V
 */
JNIEXPORT void JNICALL Java_amzi_ls_ARulesLogicServer_AddJPred
  (JNIEnv * jenv, jobject jobj, jstring jfunctor, jint arity,
   jstring jcl_name, jstring jmethod_name, jobject jmethobj,
   jlong lseng)
{
//FILE* log = fopen("d:\\temp\\a5jlog.txt", "w");
//fprintf(log, "Log file\n");

   RC  rc;
   aCHAR* functor;
   char* method_name;
   char* cl_name;
   JExtPred* ep;
   jobject jgobj;

//fprintf(log, "0\n"); fflush(log);
   GET_EID(e);

//fprintf(log, "1\n"); fflush(log);
   ep = new JExtPred();
   ep->jenv = jenv;

//fprintf(log, "2\n"); fflush(log);
   if (jmethobj == NULL)
      ep->jobj = NULL;
   else
   {
      jgobj = jenv->NewGlobalRef(jmethobj);
      ep->jobj = jgobj;
   }

   ep->lseng = lseng;
   ep->jcl = NULL;
   ep->jmeth = NULL;
   
//fprintf(log, "3\n"); fflush(log);
   // Find the class for the extended predicate
   cl_name = JtoCA(jenv, jcl_name);
   ep->jclass_name = cl_name;
//   ep->jcl = jenv->FindClass(cl_name);
//   if (ep->jcl == NULL)
//      amzi_error(jenv, e, "AddPred");
//   delete[] cl_name;

//fprintf(log, "4\n"); fflush(log);
   // And the method
   method_name = JtoCA(jenv, jmethod_name);
   ep->jmeth_name = method_name;
//   ep->jmeth = jenv->GetMethodID(ep->jcl, method_name, "()Z");
//   if (ep->jmeth == NULL)
//      amzi_error(jenv, e, "AddPred");
//   delete[] method_name;

//fprintf(log, "5\n"); fflush(log);
   // thread linked list
   ep->pnext = EpList;
   EpList = ep;

//fprintf(log, "6\n"); fflush(log);
   functor = JtoC(jenv, jfunctor);
   rc = lsAddPred(e, functor, (ARITY)arity, &::p_java, (VOIDptr)ep);
   if (rc != OK)
      amzi_error(jenv, e, "AddPred");
   delete[] functor;

//fprintf(log, "done"); fclose(log);
   return;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    AddLSX
 * Signature: (Ljava/lang/String;J)V
 */
extern "C" JNIEXPORT void JNICALL Java_amzi_ls_ARulesLogicServer_AddLSX
  (JNIEnv * jenv, jobject jobj, jstring jlsxname, jlong ptr)
{
   RC  rc;
   aCHAR* lsxname;

   GET_EID(e);
   lsxname = JtoC(jenv, jlsxname);
   rc = lsAddLSX(e, lsxname, (void *)(ajptr)ptr);
   delete[] lsxname;
   if (rc != OK)
      amzi_error(jenv, e, "AddLSX");

   return;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    Load
 * Signature: (Ljava/lang/String;)V
 */
extern "C" JNIEXPORT void JNICALL Java_amzi_ls_ARulesLogicServer_Load
  (JNIEnv * jenv, jobject jobj, jstring jstr)
{
   RC     rc;
   aCHAR* xplname;

   GET_EID(e);
   //ENGid e;
   //e = _get_eid(jenv, jobj);
   xplname = JtoC(jenv, jstr);
   rc = lsLoad(e, xplname);
   delete[] xplname;
   if (rc != OK)
      amzi_error(jenv, e, "Load");

   return;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    LoadFromMemory
 * Signature: (Ljava/lang/String;I[B)V
 */
extern "C" JNIEXPORT void JNICALL Java_amzi_ls_ARulesLogicServer_LoadFromMemory
  (JNIEnv * jenv, jobject jobj, jstring jstr, jint jlen, jbyteArray jcode)
{
   RC     rc;
   aCHAR* xplname;
   aBYTEptr code;

   GET_EID(e);
   //ENGid e;
   //e = _get_eid(jenv, jobj);
   xplname = JtoC(jenv, jstr);
   jsize len = jenv->GetArrayLength(jcode);
   jbyte *jarray = jenv->GetByteArrayElements(jcode, 0);
   code = (aBYTEptr)jarray;
   rc = lsLoadFromMemory(e, xplname, jlen, code);
   jenv->ReleaseByteArrayElements(jcode, jarray, 0);
   delete[] xplname;
   if (rc != OK)
      amzi_error(jenv, e, "LoadFromMemory");

   return;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    Main
 * Signature: ()Z
 */
extern "C" JNIEXPORT jboolean JNICALL Java_amzi_ls_ARulesLogicServer_Main
  (JNIEnv * jenv, jobject jobj)
{
   TF  tf;
   jenv_current = jenv;

   GET_EID(e);
   tf = lsMain(e);
   if (tf != TRUE && tf != FALSE)
      amzi_error(jenv, e, "Main");

   return (jboolean)tf;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    Reset
 * Signature: ()V
 */
extern "C" JNIEXPORT void JNICALL Java_amzi_ls_ARulesLogicServer_Reset
  (JNIEnv * jenv, jobject jobj)
{
   RC  rc;

   GET_EID(e);
   rc = lsReset(e);
   if (rc != OK)
      amzi_error(jenv, e, "Reset");

   return;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    JClose
 * Signature: (Ljava/lang/Object;)V
 */
JNIEXPORT void JNICALL Java_amzi_ls_ARulesLogicServer_JClose
  (JNIEnv * jenv, jobject jobj, jlong lseng)
{
   RC    rc;
   jenv_current = jenv;  // needed for JExtPred destructor

   GET_EID(e);

   JExtPred *e1 = EpList;
   JExtPred *e2;
   JExtPred *e0 = NULL;

   while(e1)
   {
      e2 = e1->pnext;
      if (e1->lseng == lseng)
      {
         if (e0)
            e0->pnext = e2;
         if (EpList == e1)
            EpList = e2;
         delete e1;
      }
      else
      {
         e0 = e1;
      }
      e1 = e2;
   }
   
   rc = lsClose(e);
   if (rc != OK)
      amzi_error(jenv, e, "Close");

   return;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    Exec
 * Signature: (J)J
 */
extern "C" JNIEXPORT jlong JNICALL Java_amzi_ls_ARulesLogicServer_Exec
  (JNIEnv * jenv, jobject jobj, jlong jterm)
{
   TF   tf;
   TERM t;

   jenv_current = jenv;
   GET_EID(e);
   t = (TERM)(ajptr)jterm;
   tf = lsExec(e, &t);
   switch(tf)
   {
   case TRUE: return (jlong)(ajptr)t;
   case FALSE: return 0;
   default: amzi_error(jenv, e, "Exec"); return 0;
   }
}

  
/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    ExecStr
 * Signature: (Ljava/lang/String;)J
 */
extern "C" JNIEXPORT jlong JNICALL Java_amzi_ls_ARulesLogicServer_ExecStr
  (JNIEnv * jenv, jobject jobj, jstring jstr)
{
   TF     tf;
   TERM   t;
   aCHAR* query;

   jenv_current = jenv;
   GET_EID(e);
   query = JtoC(jenv, jstr);
   tf = lsExecStr(e, &t, query);
   delete[] query;

   switch(tf)
   {
   case TRUE: return (jlong)(ajptr)t;
   case FALSE: return 0;
   default: amzi_error(jenv, e, "ExecStr"); return 0;
   }
}


/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    Call
 * Signature: (J)J
 */
extern "C" JNIEXPORT jlong JNICALL Java_amzi_ls_ARulesLogicServer_Call
  (JNIEnv * jenv, jobject jobj, jlong jterm)
{
   TF   tf;
   TERM t;

   jenv_current = jenv;
   GET_EID(e);
   t = (TERM)(ajptr)jterm;
   tf = lsCall(e, &t);
   switch(tf)
   {
   case TRUE: return (jlong)(ajptr)t;
   case FALSE: return 0;
   default: amzi_error(jenv, e, "Call"); return 0;
   }
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    CallStr
 * Signature: (Ljava/lang/String;)J
 */
extern "C" JNIEXPORT jlong JNICALL Java_amzi_ls_ARulesLogicServer_CallStr
  (JNIEnv * jenv, jobject jobj, jstring jstr)
{
   TF     tf;
   TERM   t;
   aCHAR* query;

   jenv_current = jenv;
   GET_EID(e);
   query = JtoC(jenv, jstr);
   tf = lsCallStr(e, &t, query);
   delete[] query;

   switch(tf)
   {
   case TRUE: return (jlong)(ajptr)t;
   case FALSE: return 0;
   default: amzi_error(jenv, e, "CallStr"); return 0;
   }
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    Redo
 * Signature: ()Z
 */
extern "C" JNIEXPORT jboolean JNICALL Java_amzi_ls_ARulesLogicServer_Redo
  (JNIEnv * jenv, jobject jobj)
{
   TF  tf;

   jenv_current = jenv;
   GET_EID(e);
   tf = lsRedo(e);
   if (tf != TRUE && tf != FALSE)
      amzi_error(jenv, e, "Redo");

   return (jboolean)tf;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    ClearCall
 * Signature: ()V
 */
extern "C" JNIEXPORT void JNICALL Java_amzi_ls_ARulesLogicServer_ClearCall
  (JNIEnv * jenv, jobject jobj)
{
   RC  rc;

   GET_EID(e);
   rc = lsClearCall(e);
   if (rc != OK)
      amzi_error(jenv, e, "ClearCall");

   return;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    Asserta
 * Signature: (J)V
 */
extern "C" JNIEXPORT void JNICALL Java_amzi_ls_ARulesLogicServer_Asserta
  (JNIEnv * jenv, jobject jobj, jlong jterm)
{
   RC  rc;

   GET_EID(e);
   rc = lsAsserta(e, (TERM)(ajptr)jterm);
   if (rc != OK)
      amzi_error(jenv, e, "Asserta");

   return;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    Assertz
 * Signature: (J)V
 */
extern "C" JNIEXPORT void JNICALL Java_amzi_ls_ARulesLogicServer_Assertz
  (JNIEnv * jenv, jobject jobj, jlong jterm)
{
   RC  rc;

   GET_EID(e);
   rc = lsAssertz(e, (TERM)(ajptr)jterm);
   if (rc != OK)
      amzi_error(jenv, e, "Assertz");

   return;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    Retract
 * Signature: (J)V
 */
extern "C" JNIEXPORT jlong JNICALL Java_amzi_ls_ARulesLogicServer_Retract
  (JNIEnv * jenv, jobject jobj, jlong jterm)
{
   TF   tf;
   TERM t;

   GET_EID(e);
   t = (TERM)(ajptr)jterm;
   tf = lsRetract(e, t);
   switch(tf)
   {
   case TRUE: return (jlong)(ajptr)t;
   case FALSE: return 0;
   default: amzi_error(jenv, e, "Retract"); return 0;
   }
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    AssertaStr
 * Signature: (Ljava/lang/String;)V
 */
extern "C" JNIEXPORT void JNICALL Java_amzi_ls_ARulesLogicServer_AssertaStr
  (JNIEnv * jenv, jobject jobj, jstring jstr)
{
   RC     rc;
   aCHAR* astr;

   GET_EID(e);
   astr = JtoC(jenv, jstr);
   rc = lsAssertaStr(e, astr);
   delete[] astr;
   if (rc != OK)
      amzi_error(jenv, e, "AssertaStr");

   return;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    AssertzStr
 * Signature: (Ljava/lang/String;)V
 */
extern "C" JNIEXPORT void JNICALL Java_amzi_ls_ARulesLogicServer_AssertzStr
  (JNIEnv * jenv, jobject jobj, jstring jstr)
{
   RC     rc;
   aCHAR* astr;

   GET_EID(e);
   astr = JtoC(jenv, jstr);
   rc = lsAssertzStr(e, astr);
   delete[] astr;
   if (rc != OK)
      amzi_error(jenv, e, "AssertzStr");

   return;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    RetractStr
 * Signature: (Ljava/lang/String;)V
 */
extern "C" JNIEXPORT jboolean JNICALL Java_amzi_ls_ARulesLogicServer_RetractStr
  (JNIEnv * jenv, jobject jobj, jstring jstr)
{
   TF   tf;
   aCHAR* rstr;

   GET_EID(e);
   rstr = JtoC(jenv, jstr);
   tf = lsRetractStr(e, rstr);
   delete[] rstr;
   switch(tf)
   {
   case TRUE:
   case FALSE:
      return (jboolean)tf;
   default: amzi_error(jenv, e, "RetractStr"); return 0;
   }
}

  
/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    TermToStr
 * Signature: (JI)Ljava/lang/String;
 */
extern "C" JNIEXPORT jstring JNICALL Java_amzi_ls_ARulesLogicServer_TermToStr
  (JNIEnv * jenv, jobject jobj, jlong t, jint jlen)
{
   RC     rc;
   aCHAR* str = new aCHAR[jlen+1];

   GET_EID(e);
   rc = lsTermToStr(e, (TERM)(ajptr)t, str, jlen);
   int len = wcslen(str);
   jCHAR *jcstr = new jCHAR[len+1];
   ac_to_jc(jcstr, str, len);
   jstring jstr = jenv->NewString(jcstr, len);
   delete[] str;
   delete[] jcstr;
   if (rc != OK)
      amzi_error(jenv, e, "TermToStr");

   return jstr;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    TermToStrQ
 * Signature: (JI)Ljava/lang/String;
 */
extern "C" JNIEXPORT jstring JNICALL Java_amzi_ls_ARulesLogicServer_TermToStrQ
  (JNIEnv * jenv, jobject jobj, jlong t, jint jlen)
{
   RC     rc;
   aCHAR* str = new aCHAR[jlen+1];

   GET_EID(e);
   rc = lsTermToStrQ(e, (TERM)(ajptr)t, str, jlen);
   int len = wcslen(str);
   jCHAR* jcstr = new jCHAR[len+1];
   ac_to_jc(jcstr, str, len);
   jstring jstr = jenv->NewString(jcstr, len);
   delete[] str;
   if (rc != OK)
      amzi_error(jenv, e, "TermToStrQ");

   return jstr;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    StrToTerm
 * Signature: (Ljava/lang/String;)J
 */
extern "C" JNIEXPORT jlong JNICALL Java_amzi_ls_ARulesLogicServer_StrToTerm
  (JNIEnv * jenv, jobject jobj, jstring jstr)
{
   RC     rc;
   TERM   t;
   aCHAR* istr;

   GET_EID(e);
   istr = JtoC(jenv, jstr);
   rc = lsStrToTerm(e, &t, istr);
   delete[] istr;

   if (rc != OK)
      amzi_error(jenv, e, "StrToTerm");
   
   return (jlong)(ajptr)t;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    StrTermLen
 * Signature: (J)J
 */
extern "C" JNIEXPORT jint JNICALL Java_amzi_ls_ARulesLogicServer_StrTermLen
  (JNIEnv * jenv, jobject jobj, jlong jterm)
{
   int len;

   GET_EID(e);
   len = lsStrTermLen(e, (TERM)(ajptr)jterm);

   return (jint)len;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    MakeAtom
 * Signature: (Ljava/lang/String;)J
 */
extern "C" JNIEXPORT jlong JNICALL Java_amzi_ls_ARulesLogicServer_MakeAtom
  (JNIEnv * jenv, jobject jobj, jstring jstr)
{
   RC     rc;
   aCHAR* atom;
   TERM   t;

   GET_EID(e);
   atom = JtoC(jenv, jstr);
   rc = lsMakeAtom(e, &t, atom);
   delete[] atom;

   if (rc != OK)
      amzi_error(jenv, e, "MakeAtom");

   return (jlong)(ajptr)t;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    MakeStr
 * Signature: (Ljava/lang/String;)J
 */
extern "C" JNIEXPORT jlong JNICALL Java_amzi_ls_ARulesLogicServer_MakeStr
  (JNIEnv * jenv, jobject jobj, jstring jstr)
{
   RC     rc;
   aCHAR* str;
   TERM   t;

   GET_EID(e);
   str = JtoC(jenv, jstr);
   rc = lsMakeStr(e, &t, str);
   delete[] str;

   if (rc != OK)
      amzi_error(jenv, e, "MakeStr");

   return (jlong)(ajptr)t;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    MakeInt
 * Signature: (I)J
 */
extern "C" JNIEXPORT jlong JNICALL Java_amzi_ls_ARulesLogicServer_MakeInt
  (JNIEnv * jenv, jobject jobj, jint ji)
{
   RC     rc;
   TERM   t;

   GET_EID(e);
   rc = lsMakeInt(e, &t, (int)ji);

   if (rc != OK)
      amzi_error(jenv, e, "MakeInt");

   return (jlong)(ajptr)t;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    MakeFloat
 * Signature: (D)J
 */
extern "C" JNIEXPORT jlong JNICALL Java_amzi_ls_ARulesLogicServer_MakeFloat
  (JNIEnv * jenv, jobject jobj, jdouble jd)
{
   RC     rc;
   TERM   t;

   GET_EID(e);
   rc = lsMakeFloat(e, &t, (double)jd);

   if (rc != OK)
      amzi_error(jenv, e, "MakeFloat");

   return (jlong)(ajptr)t;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    GetTermType
 * Signature: (J)I
 */
extern "C" JNIEXPORT jint JNICALL Java_amzi_ls_ARulesLogicServer_GetTermType
  (JNIEnv * jenv, jobject jobj, jlong jterm)
{
   pTYPE type;

   GET_EID(e);
   type = lsGetTermType(e, (TERM)(ajptr)jterm);
   if (type < 0)
      amzi_error(jenv, e, "GetTermType");

   return (jint)type;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    GetStrTerm
 * Signature: (J)Ljava/lang/String;
 */
extern "C" JNIEXPORT jstring JNICALL Java_amzi_ls_ARulesLogicServer_GetStrTerm
  (JNIEnv * jenv, jobject jobj, jlong jt)
{
   RC     rc;
   aCHAR* str;

   GET_EID(e);
   int len = lsStrTermLen(e, (TERM)(ajptr)jt);

   if (len < 0)                             // jmg
       len = 0;                             // jmg 

   str = new aCHAR[len+1];
   rc = lsGetTerm(e, (TERM)(ajptr)jt, cWSTR, str);

   if (rc != OK)                            // jmg
       *str = 0;                            // jmg

   jCHAR *jcstr = new jCHAR[len+1];
   ac_to_jc(jcstr, str, len);
   jstring jstr = jenv->NewString(jcstr, len);
   delete[] str;
   delete[] jcstr;
   if (rc != OK)
      amzi_error(jenv, e, "GetStrTerm");

   return jstr;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    GetIntTerm
 * Signature: (J)I
 */
extern "C" JNIEXPORT jint JNICALL Java_amzi_ls_ARulesLogicServer_GetIntTerm
  (JNIEnv * jenv, jobject jobj, jlong jt)
{
   RC     rc;
   int    i;

   GET_EID(e);
   rc = lsGetTerm(e, (TERM)(ajptr)jt, cINT, &i);
   if (rc != OK)
      amzi_error(jenv, e, "GetIntTerm");

   return (jint)i;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    GetFloatTerm
 * Signature: (J)D
 */
extern "C" JNIEXPORT jdouble JNICALL Java_amzi_ls_ARulesLogicServer_GetFloatTerm
  (JNIEnv * jenv, jobject jobj, jlong jt)
{
   RC     rc;
   double d;

   GET_EID(e);
   rc = lsGetTerm(e, (TERM)(ajptr)jt, cDOUBLE, &d);
   if (rc != OK)
      amzi_error(jenv, e, "GetFloatTerm");

   return (jdouble)d;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    GetParmType
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_amzi_ls_ARulesLogicServer_GetParmType
  (JNIEnv * jenv, jobject jobj, jint iarg)
{
   pTYPE type;

   GET_EID(e);
   type = lsGetParmType(e, (int)iarg);
   if (type < 0)
      amzi_error(jenv, e, "GetTermType");

   return (jint)type;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    GetParm
 * Signature: (I)J
 */
JNIEXPORT jlong JNICALL Java_amzi_ls_ARulesLogicServer_GetParm
  (JNIEnv * jenv, jobject jobj, jint iarg)
{
   TERM t;
   RC   rc;

   GET_EID(e);
   rc = lsGetParm(e, (int)iarg, cTERM, &t);
   if (rc != OK)
      amzi_error(jenv, e, "GetParm");

   return (jlong)(ajptr)t;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    GetStrParm
 * Signature: (I)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_amzi_ls_ARulesLogicServer_GetStrParm
  (JNIEnv * jenv, jobject jobj, jint iarg)
{
   RC   rc;
   aCHAR* str;

   GET_EID(e);
   int len = lsStrParmLen(e, (int)iarg);

   if (len < 0)                         // jmg 
       len = 0;                         // jmg

   str = new aCHAR[len+1];
   rc = lsGetParm(e, (int)iarg, cWSTR, str);

   if (rc != OK)                        // jmg
       *str = 0;                        // jmg

   jCHAR *jcstr = new jCHAR[len+1];
   ac_to_jc(jcstr, str, len);
   jstring jstr = jenv->NewString(jcstr, len);
   delete[] str;
   delete[] jcstr;
   if (rc != OK)
      amzi_error(jenv, e, "GetStrParm");

   return jstr;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    GetIntParm
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_amzi_ls_ARulesLogicServer_GetIntParm
  (JNIEnv * jenv, jobject jobj, jint iarg)
{
   RC     rc;
   int    i;

   GET_EID(e);
   rc = lsGetParm(e, (int)iarg, cINT, &i);
   if (rc != OK)
      amzi_error(jenv, e, "GetIntParm");

   return (jint)i;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    GetFloatParm
 * Signature: (I)D
 */
JNIEXPORT jdouble JNICALL Java_amzi_ls_ARulesLogicServer_GetFloatParm
  (JNIEnv * jenv, jobject jobj, jint iarg)
{
   RC     rc;
   double d;

   GET_EID(e);
   rc = lsGetParm(e, (int)iarg, cDOUBLE, &d);
   if (rc != OK)
      amzi_error(jenv, e, "GetFloatParm");

   return (jdouble)d;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    UnifyParm
 * Signature: (IJ)Z
 */
JNIEXPORT jboolean JNICALL Java_amzi_ls_ARulesLogicServer_UnifyParm
  (JNIEnv * jenv, jobject jobj, jint iarg, jlong term)
{
   TF tf;

   GET_EID(e);
   tf = lsUnifyParm(e, (int)iarg, cTERM, &term);
   switch(tf)
   {
   case TRUE:
   case FALSE:
      return (jboolean)tf;
   default: amzi_error(jenv, e, "UnifyParm"); return 0;
   }
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    UnifyStrParm
 * Signature: (ILjava/lang/String;)Z
 */
JNIEXPORT jboolean JNICALL Java_amzi_ls_ARulesLogicServer_UnifyStrParm
  (JNIEnv * jenv, jobject jobj, jint iarg, jstring jstr)
{
   TF tf;
   aCHAR* str;

   GET_EID(e);
   str = JtoC(jenv, jstr);
   tf = lsUnifyParm(e, (int)iarg, cWSTR, str);
   delete[] str;

   switch(tf)
   {
   case TRUE:
   case FALSE:
      return (jboolean)tf;
   default: amzi_error(jenv, e, "UnifyStrParm"); return 0;
   }
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    UnifyAtomParm
 * Signature: (ILjava/lang/String;)Z
 */
JNIEXPORT jboolean JNICALL Java_amzi_ls_ARulesLogicServer_UnifyAtomParm
  (JNIEnv * jenv, jobject jobj, jint iarg, jstring jstr)
{
   TF tf;
   aCHAR* str;

   GET_EID(e);
   str = JtoC(jenv, jstr);
   tf = lsUnifyParm(e, (int)iarg, cWATOM, str);
   delete[] str;

   switch(tf)
   {
   case TRUE:
   case FALSE:
      return (jboolean)tf;
   default: amzi_error(jenv, e, "UnifyAtomParm"); return 0;
   }
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    UnifyIntParm
 * Signature: (II)Z
 */
JNIEXPORT jboolean JNICALL Java_amzi_ls_ARulesLogicServer_UnifyIntParm
  (JNIEnv * jenv, jobject jobj, jint iarg, jint ji)
{
   TF tf;

   GET_EID(e);
   tf = lsUnifyParm(e, (int)iarg, cINT, &ji);

   switch(tf)
   {
   case TRUE:
   case FALSE:
      return (jboolean)tf;
   default: amzi_error(jenv, e, "UnifyIntParm"); return 0;
   }
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    UnifyFloatParm
 * Signature: (ID)Z
 */
JNIEXPORT jboolean JNICALL Java_amzi_ls_ARulesLogicServer_UnifyFloatParm
  (JNIEnv * jenv, jobject jobj, jint iarg, jdouble jd)
{
   TF tf;

   GET_EID(e);
   tf = lsUnifyParm(e, (int)iarg, cDOUBLE, &jd);

   switch(tf)
   {
   case TRUE:
   case FALSE:
      return (jboolean)tf;
   default: amzi_error(jenv, e, "UnifyFloatParm"); return 0;
   }
}


/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    GetFunctor
 * Signature: (J)Ljava/lang/String;
 */
extern "C" JNIEXPORT jstring JNICALL Java_amzi_ls_ARulesLogicServer_GetFunctor
  (JNIEnv * jenv, jobject jobj, jlong jt)
{
   RC     rc;
   aCHAR  str[1024];
   jCHAR  jcstr[1024];
   ARITY  a;

   GET_EID(e);
   rc = lsGetFA(e, (TERM)(ajptr)jt, str, &a);
   if (rc != OK)
      amzi_error(jenv, e, "GetFunctor");

   int len = wcslen(str);
   ac_to_jc(jcstr, str, len);
   jstring jstr = jenv->NewString(jcstr, len);

   return jstr;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    GetArity
 * Signature: (J)S
 */
extern "C" JNIEXPORT jint JNICALL Java_amzi_ls_ARulesLogicServer_GetArity
  (JNIEnv * jenv, jobject jobj, jlong jt)
{
   RC     rc;
   aCHAR  str[1024];
   ARITY  a;

   GET_EID(e);
   rc = lsGetFA(e, (TERM)(ajptr)jt, str, &a);
   if (rc != OK)
      amzi_error(jenv, e, "GetArity");

   return (jint)a;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    MakeFA
 * Signature: (Ljava/lang/String;I)J
 */
extern "C" JNIEXPORT jlong JNICALL Java_amzi_ls_ARulesLogicServer_MakeFA
  (JNIEnv * jenv, jobject jobj, jstring jstr, jint ja)
{
   RC     rc;
   TERM   t;
   aCHAR* str;

   GET_EID(e);
   str = JtoC(jenv, jstr);
   rc = lsMakeFA(e, &t, str, (ARITY)ja);
   delete[] str;

   if (rc != OK)
      amzi_error(jenv, e, "MakeFA");
   
   return (jlong)(ajptr)t;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    GetArg
 * Signature: (JI)J
 */
extern "C" JNIEXPORT jlong JNICALL Java_amzi_ls_ARulesLogicServer_GetArg
  (JNIEnv * jenv, jobject jobj, jlong jt, jint ji)
{
   RC   rc;
   TERM t;

   GET_EID(e);
   rc = lsGetArg(e, (TERM)(ajptr)jt, (int)ji, cTERM, &t);
   if (rc != OK)
      amzi_error(jenv, e, "GetArg");

   return (jlong)(ajptr)t;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    GetStrArg
 * Signature: (JI)Ljava/lang/String;
 */
extern "C" JNIEXPORT jstring JNICALL Java_amzi_ls_ARulesLogicServer_GetStrArg
  (JNIEnv * jenv, jobject jobj, jlong t, jint iarg)
{
   RC     rc;

   GET_EID(e);
   int len = lsStrArgLen(e, (TERM)(ajptr)t, (int)iarg);

   if (len < 0)             // jmg
       len = 0;             // jmg

   aCHAR* str = new aCHAR[len+1];
   rc = lsGetArg(e, (TERM)(ajptr)t, (int)iarg, cWSTR, str);

   if (rc != OK)            // jmg - if the Prolog engine was not able to convert, make
       *str = 0;            // jmg - sure str is empty.  rc contains a proper errcode.

   jCHAR* jcstr = new jCHAR[len+1];
   ac_to_jc(jcstr, str, len);
   jstring jstr = jenv->NewString(jcstr, len);
   delete[] str;
   delete[] jcstr;
   if (rc != OK)
      amzi_error(jenv, e, "GetStrArg");

   return jstr;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    GetIntArg
 * Signature: (JI)I
 */
extern "C" JNIEXPORT jint JNICALL Java_amzi_ls_ARulesLogicServer_GetIntArg
  (JNIEnv * jenv, jobject jobj, jlong jt, jint ji)
{
   RC   rc;
   int  i;

   GET_EID(e);
   rc = lsGetArg(e, (TERM)(ajptr)jt, (int)ji, cINT, &i);
   if (rc != OK)
      amzi_error(jenv, e, "GetIntArg");

   return (jint)i;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    GetFloatArg
 * Signature: (JI)D
 */
extern "C" JNIEXPORT jdouble JNICALL Java_amzi_ls_ARulesLogicServer_GetFloatArg
  (JNIEnv * jenv, jobject jobj, jlong jt, jint ji)
{
   RC   rc;
   double d;

   GET_EID(e);
   rc = lsGetArg(e, (TERM)(ajptr)jt, (int)ji, cDOUBLE, &d);
   if (rc != OK)
      amzi_error(jenv, e, "GetFloatArg");

   return (jdouble)d;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    UnifyAtomArg
 * Signature: (JILjava/lang/String;)J
 */
extern "C" JNIEXPORT jlong JNICALL Java_amzi_ls_ARulesLogicServer_UnifyAtomArg
  (JNIEnv * jenv, jobject jobj, jlong jt, jint ji, jstring jstr)
{
   TF     tf;
   TERM   t;
   aCHAR* str;

   GET_EID(e);
   t = (TERM)(ajptr)jt;
   str = JtoC(jenv, jstr);
   tf = lsUnifyArg(e, &t, (int)ji, cWATOM, str); 
   delete[] str;

   switch(tf)
   {
   case TRUE: return (jlong)(ajptr)t;
   case FALSE: return 0;
   default: amzi_error(jenv, e, "UnifyAtomArg"); return 0;
   }
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    UnifyStrArg
 * Signature: (JILjava/lang/String;)J
 */
extern "C" JNIEXPORT jlong JNICALL Java_amzi_ls_ARulesLogicServer_UnifyStrArg
  (JNIEnv * jenv, jobject jobj, jlong jt, jint ji, jstring jstr)
{
   TF     tf;
   TERM   t;
   aCHAR* str;

   GET_EID(e);
   t = (TERM)(ajptr)jt;
   str = JtoC(jenv, jstr);
   tf = lsUnifyArg(e, &t, (int)ji, cWSTR, str); 
   delete[] str;

   switch(tf)
   {
   case TRUE: return (jlong)(ajptr)t;
   case FALSE: return 0;
   default: amzi_error(jenv, e, "UnifyStrArg"); return 0;
   }
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    UnifyIntArg
 * Signature: (JII)J
 */
extern "C" JNIEXPORT jlong JNICALL Java_amzi_ls_ARulesLogicServer_UnifyIntArg
  (JNIEnv * jenv, jobject jobj, jlong jt, jint jarg, jint ji)
{
   TF     tf;
   TERM   t;
   int    i;

   GET_EID(e);
   t = (TERM)(ajptr)jt;
   i = (int)ji;
   tf = lsUnifyArg(e, &t, (int)jarg, cINT, &i); 

   switch(tf)
   {
   case TRUE: return (jlong)(ajptr)t;
   case FALSE: return 0;
   default: amzi_error(jenv, e, "UnifyIntArg"); return 0;
   }
}


/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    UnifyFloatArg
 * Signature: (JID)J
 */
extern "C" JNIEXPORT jlong JNICALL Java_amzi_ls_ARulesLogicServer_UnifyFloatArg
  (JNIEnv * jenv, jobject jobj, jlong jt, jint jarg, jdouble jd)
{
   TF     tf;
   TERM   t;
   double d;

   GET_EID(e);
   t = (TERM)(ajptr)jt;
   d = (double)jd;
   tf = lsUnifyArg(e, &t, (int)jarg, cDOUBLE, &d); 

   switch(tf)
   {
   case TRUE: return (jlong)(ajptr)t;
   case FALSE: return 0;
   default: amzi_error(jenv, e, "UnifyFloatArg"); return 0;
   }
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    GetArgType
 * Signature: (JI)S
 */
extern "C" JNIEXPORT jint JNICALL Java_amzi_ls_ARulesLogicServer_GetArgType
  (JNIEnv * jenv, jobject jobj, jlong jt, jint ji)
{
   pTYPE type;

   GET_EID(e);
   type = lsGetArgType(e, (TERM)(ajptr)jt, (int)ji);

   return (jint)type;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    StrArgLen
 * Signature: (JI)I
 */
extern "C" JNIEXPORT jint JNICALL Java_amzi_ls_ARulesLogicServer_StrArgLen
  (JNIEnv * jenv, jobject jobj, jlong jt, jint ji)
{
   int len;

   GET_EID(e);
   len = lsStrArgLen(e, (TERM)(ajptr)jt, (int)ji);

   return (jint)len;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    MakeList
 * Signature: ()J
 */
extern "C" JNIEXPORT jlong JNICALL Java_amzi_ls_ARulesLogicServer_MakeList
  (JNIEnv * jenv, jobject jobj)
{
   RC    rc;
   TERM  t;

   GET_EID(e);
   rc = lsMakeList(e, &t);
   if (rc != OK)
      amzi_error(jenv, e, "MakeList");

   return (jlong)(ajptr)t;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    PushList
 * Signature: (JJ)J
 */
extern "C" JNIEXPORT jlong JNICALL Java_amzi_ls_ARulesLogicServer_PushList
  (JNIEnv * jenv, jobject jobj, jlong jt, jlong jlt)
{
   RC     rc;
   TERM   t;

   GET_EID(e);
   t = (TERM)(ajptr)jt;
   rc = lsPushList(e, &t, (TERM)(ajptr)jlt);
   if (rc != OK)
      amzi_error(jenv, e, "PushList");

   return (jlong)(ajptr)t;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    GetHead
 * Signature: (J)J
 */
extern "C" JNIEXPORT jlong JNICALL Java_amzi_ls_ARulesLogicServer_GetHead
  (JNIEnv * jenv, jobject jobj, jlong jt)
{
   RC     rc;
   TERM   t;

   GET_EID(e);
   rc = lsGetHead(e, (TERM)(ajptr)jt, cTERM, &t);
   if (rc != OK)
      amzi_error(jenv, e, "GetHead");

   return (jlong)(ajptr)t;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    GetStrHead
 * Signature: (J)Ljava/lang/String;
 */
extern "C" JNIEXPORT jstring JNICALL Java_amzi_ls_ARulesLogicServer_GetStrHead
  (JNIEnv * jenv, jobject jobj, jlong jt)
{
   RC     rc;
   aCHAR* str;
   jCHAR* jcstr;
   TERM   t;

   GET_EID(e);
   rc = lsGetHead(e, (TERM)(ajptr)jt, cTERM, &t);
   if (rc != OK)
      amzi_error(jenv, e, "GetStrHead");

   int len = lsStrTermLen(e, t);

   if (len < 0)                     // jmg 
       len = 0;                     // jmg

   str = new aCHAR[len+1];
   rc = lsGetTerm(e, t, cWSTR, str);

   if (rc != OK)                    // jmg
       *str = 0;                    // jmg

   jcstr = new jCHAR[len+1];
   ac_to_jc(jcstr, str, len);
   jstring jstr = jenv->NewString(jcstr, len);
   delete[] str;
   delete[] jcstr;
   if (rc != OK)
      amzi_error(jenv, e, "GetStrHead");

   return jstr;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    GetIntHead
 * Signature: (I)S
 */
extern "C" JNIEXPORT jint JNICALL Java_amzi_ls_ARulesLogicServer_GetIntHead
  (JNIEnv * jenv, jobject jobj, jlong jt)
{
   RC     rc;
   int    i;

   GET_EID(e);
   rc = lsGetHead(e, (TERM)(ajptr)jt, cINT, &i);
   if (rc != OK)
      amzi_error(jenv, e, "GetIntHead");

   return (jint)i;
}


/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    GetFloatHead
 * Signature: (J)D
 */
extern "C" JNIEXPORT jdouble JNICALL Java_amzi_ls_ARulesLogicServer_GetFloatHead
  (JNIEnv * jenv, jobject jobj, jlong jt)
{
   RC     rc;
   double d;

   GET_EID(e);
   rc = lsGetHead(e, (TERM)(ajptr)jt, cDOUBLE, &d);
   if (rc != OK)
      amzi_error(jenv, e, "GetFloatHead");

   return (jdouble)d;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    GetTail
 * Signature: (J)J
 */
extern "C" JNIEXPORT jlong JNICALL Java_amzi_ls_ARulesLogicServer_GetTail
  (JNIEnv * jenv, jobject jobj, jlong jt)
{
   TERM   t;

   GET_EID(e);
   t = lsGetTail(e, (TERM)(ajptr)jt);

   return (jlong)(ajptr)t;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    GetVersion
 * Signature: ()Ljava/lang/String;
 */
extern "C" JNIEXPORT jstring JNICALL Java_amzi_ls_ARulesLogicServer_GetVersion
  (JNIEnv * jenv, jobject jobj)
{
   RC     rc;
   aCHAR  str[1024];
   jCHAR  jcstr[1024];

   GET_EID(e);
   rc = lsGetVersion(e, str);
   if (rc != OK)
      amzi_error(jenv, e, "GetVersion");

   int len = wcslen(str);
   ac_to_jc(jcstr, str, len);
   jstring jstr = jenv->NewString(jcstr, len);

   return jstr;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    ARulesRegisterRuntime
 * Signature: (JLjava/lang/String;Ljava/lang/String;)I
 */
extern "C" JNIEXPORT jint JNICALL Java_amzi_ls_ARulesLogicServer_ARulesRegisterRuntime
  (JNIEnv *jenv, jobject jobj, jlong method, jstring proxylist, jstring runtimeid)
{
   RC rc;
   char* pstr;
   char* rstr;

   GET_EID(e);
   pstr = JtoCA(jenv, proxylist);
   rstr = JtoCA(jenv, runtimeid);
   rc = RegisterRuntime(e, method, pstr, rstr);
   delete[] pstr;
   delete[] rstr;
   return rc;
}


//-------------------------------------------------
// Support for redirecting Prolog streams
//

JExtPred* jioList = NULL;

aCHAR* EXPFUNC juser_getline(void* p)
// AddPred will add as its predicate:
//		AddPred("predname", arity, &::p_java, ptr to the JExtPred block);
{
   jstring js;
   JExtPred* ep = (JExtPred*)p;
   
   // don't use saved one
   //ep->jenv = jenv_current;

   if (ep->jobj == NULL)
   {
      if (ep->jcl == NULL)
         ep->jcl = ep->jenv->FindClass(ep->jclass_name);
      ep->jmeth = ep->jenv->GetStaticMethodID(ep->jcl, ep->jmeth_name, "()Ljava/lang/String;");
      if (ep->jmeth == NULL)
         return false;
      js = (jstring)(ep->jenv->CallStaticObjectMethod(ep->jcl, ep->jmeth));
   }
   else
   {
      //ep->jcl = ep->jenv->FindClass(ep->jclass_name);
      ep->jcl = ep->jenv->GetObjectClass(ep->jobj);
      ep->jmeth = ep->jenv->GetMethodID(ep->jcl, ep->jmeth_name, "()Ljava/lang/String;");
      if (ep->jmeth == NULL)
         return false;
      js = (jstring)(ep->jenv->CallObjectMethod(ep->jobj, ep->jmeth));
   }

   return JtoC(ep->jenv, js);
}

void EXPFUNC juser_putstring(void* p, aCHAR* cs)
// AddPred will add as its predicate:
//		AddPred("predname", arity, &::p_java, ptr to the JExtPred block);
{
   JExtPred* ep = (JExtPred*)p;
   
   // don't use saved one
   //ep->jenv = jenv_current;

   int len = wcslen(cs);
   jCHAR *jcs = new jCHAR[len+1];
   ac_to_jc(jcs, cs, len);
   jstring js = ep->jenv->NewString(jcs, len);
   delete jcs;

   // for some reason, calling FindClass twice yields a
   // null the second time, but I recollect problems with
   // threads doing it this way.  hmmmm.

   if (ep->jobj == NULL)
   {
      if (ep->jcl == NULL)
         ep->jcl = ep->jenv->FindClass(ep->jclass_name);
      ep->jmeth = ep->jenv->GetStaticMethodID(ep->jcl, ep->jmeth_name, "(Ljava/lang/String;)V");
      if (ep->jmeth == NULL)
         return;
      ep->jenv->CallStaticVoidMethod(ep->jcl, ep->jmeth, js);
   }
   else
   {
      //ep->jcl = ep->jenv->FindClass(ep->jclass_name);
      ep->jcl = ep->jenv->GetObjectClass(ep->jobj);
      ep->jmeth = ep->jenv->GetMethodID(ep->jcl, ep->jmeth_name, "(Ljava/lang/String;)V");
      if (ep->jmeth == NULL)
         return;
      ep->jenv->CallVoidMethod(ep->jobj, ep->jmeth, js);
   }

   return;
}


/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    OpenJStream
 * Signature: (ILjava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/Object;J)I
 */
extern "C" JNIEXPORT jint JNICALL Java_amzi_ls_ARulesLogicServer_OpenJStream
  (JNIEnv *jenv, jobject jobj, jint inout,
   jstring jalias, jstring jcl_name, jstring jmethod_name,
   jobject jmethobj, jlong lseng)
{
   GET_EID(e);

   int stream;
   char* method_name;
   char* cl_name;
   JExtPred* ep;
   jobject jgobj;


   ep = new JExtPred();
   ep->jenv = jenv;

   if (jmethobj == NULL)
      ep->jobj = NULL;
   else
   {
      jgobj = jenv->NewGlobalRef(jmethobj);
      ep->jobj = jgobj;
   }

   ep->lseng = lseng;
   
   // Find the class for the extended predicate
   cl_name = JtoCA(jenv, jcl_name);
   ep->jclass_name = cl_name;
   // And the method
   method_name = JtoCA(jenv, jmethod_name);
   ep->jmeth_name = method_name;
   // thread linked list
   ep->pnext = jioList;
   jioList = ep;

   ep->jcl = NULL;
   ep->jmeth = NULL;

   aCHAR *alias = JtoC(jenv, jalias);
   if (inout == 0)
      stream = lsOpenUserStream(e, alias, &::juser_getline, NULL, (VOIDptr)ep);
   else
      stream = lsOpenUserStream(e, alias, NULL, &::juser_putstring, (VOIDptr)ep);

   delete alias;

   return stream;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    CloseJStream
 * Signature: (IJ)V
 */
JNIEXPORT void JNICALL Java_amzi_ls_ARulesLogicServer_CloseJStream
  (JNIEnv * jenv, jobject jobj, jint stream, jlong lseng)
{
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    SetStream
 * Signature: (II)V
 */
JNIEXPORT void JNICALL Java_amzi_ls_ARulesLogicServer_SetStream
  (JNIEnv * jenv, jobject jobj, jint std_stream, jint stream)
{
   RC     rc;

   GET_EID(e);
   rc = lsSetStream(e, (STREAM)std_stream, (int)stream);
   if (rc != OK)
      amzi_error(jenv, e, "SetStream");

   return;
}

#if defined(_WINDOWS)
#define CLNKPAS __cdecl
typedef int (CLNKPAS *pfLINK)(void(*)(aCHAR*), int, aCHAR **);
#endif

void disp_msg(aCHAR * msg)
{
   wcscat(MsgBuf, msg);
   wcscat(MsgBuf, aS("\n"));
}

void clear_msg()
{
   MsgBuf[0] = 0;
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    Link
 * Signature: (Ljava/lang/String;[Ljava/lang/String;Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_amzi_ls_ARulesLogicServer_Link
  (JNIEnv * jenv, jobject jobj, jstring jxpl, jobjectArray jplms, jstring joptions)
{
#if defined(_WINDOWS)
   //wcscpy(MsgBuf, aS("Link not implemented yet"));
   //return -1;

//   HINSTANCE m_hLinkDLL;
//   pfLINK    m_pfL;
   
   clear_msg();

/*   m_hLinkDLL = LoadLibraryExA("alnk.dll", NULL, 0);
   if (m_hLinkDLL != NULL)
   {
      m_pfL = (pfLINK)GetProcAddress(m_hLinkDLL, "cpLinkW");
      if (m_pfL == NULL)
      {
         wsprintf(MsgBuf, aS("Unable to find Linker entry point: %d"), GetLastError());
         return -2;
      }
   }
   else
   {
      m_pfL = NULL;
      wsprintf(MsgBuf, aS("Unable to load Linker library: %d"), GetLastError());
      return -3;
   }
*/
   jsize jlen, ji;
   int i, argctr;
   aCHAR * pargv[128];
   jobject jplm;

   pargv[0] = JtoC(jenv, jxpl);
   pargv[1] = new aCHAR[1 + wcslen(aS("alib.plm"))];
   wcscpy(pargv[1], aS("alib.plm"));

   jlen = jenv->GetArrayLength(jplms);
   argctr = jlen + 2;

   for (ji=0; ji<jlen; ji++)
   {
      jplm = jenv->GetObjectArrayElement(jplms, ji);
      i = ji + 2;
      pargv[i] = JtoC(jenv, (jstring)jplm);
   }
   // Call the linker with the list of files to link
//   int rc = (*m_pfL)(disp_msg, argctr, (aCHAR **)pargv);
   int rc = aLinkW(disp_msg, argctr, (aCHAR **)pargv);

   for (i=0; i<argctr; i++)
      delete pargv[i];

//   FreeLibrary(m_hLinkDLL);
   //wsprintf(MsgBuf, aS("Link successful"));

   return rc;
#else

   jsize jlen, ji;
   int i, argctr;
   aCHAR * pargv[128];
   jobject jplm;

   pargv[0] = JtoC(jenv, jxpl);
   pargv[1] = new aCHAR[1 + wcslen(aS("alib.plm"))];
   wcscpy(pargv[1], aS("alib.plm"));

   jlen = jenv->GetArrayLength(jplms);
   argctr = jlen + 2;

   for (ji=0; ji<jlen; ji++)
   {
      jplm = jenv->GetObjectArrayElement(jplms, ji);
      i = ji + 2;
      pargv[i] = JtoC(jenv, (jstring)jplm);
   }
   // Call the linker with the list of files to link
   int rc = aLinkW(disp_msg, argctr, (aCHAR **)pargv);

   for (i=0; i<argctr; i++)
      delete pargv[i];

   return rc;

   //   wcscpy(MsgBuf, aS("Link not implemented yet"));
   //   return -1;
#endif
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    getEnvironmentVariable
 * Signature: (Ljava/lang/String;)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_amzi_ls_ARulesLogicServer_getEnvironmentVariable
  (JNIEnv * jenv, jobject jobj, jstring jenvvar)
{
#if defined(_WINDOWS)
   char val[1024] = "";
   char *var = JtoCA(jenv, jenvvar);
   DWORD cnt = GetEnvironmentVariableA(var, val, 1024);
   int len = strlen(val);
   jCHAR *jcstr = new jCHAR[len+1];
   a_to_jc(jcstr, val, len);
   jstring jstr = jenv->NewString(jcstr, len);
   delete var;
   delete jcstr;
   return jstr; 
#else
   char *var = JtoCA(jenv, jenvvar);
   char *val = getenv(var);
   int len = strlen(val);
   jCHAR *jcstr = new jCHAR[len+1];
   a_to_jc(jcstr, val, len);
   jstring jstr = jenv->NewString(jcstr, len);
   delete var;
   delete jcstr;
   return jstr;
#endif
}

/*
 * Class:     amzi_ls_ARulesLogicServer
 * Method:    getLinkMessage
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_amzi_ls_ARulesLogicServer_getLinkMessage
  (JNIEnv * jenv, jobject jobj)
{
   int len = wcslen(MsgBuf);
   ac_to_jc(jcMsgBuf, MsgBuf, len);
   jstring jstr = jenv->NewString(jcMsgBuf, len);
   MsgBuf[0] = 0;
   return jstr;
}
