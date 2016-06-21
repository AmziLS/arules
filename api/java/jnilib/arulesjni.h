/* Linked list of extended predicates */

const int MAXMSG = 100000; // Maximum error msg length

typedef jint ajptr;  // an int used as a pointer

class JExtPred
{
public:
   JNIEnv*    jenv;
   jclass     jcl;
   jmethodID  jmeth;
   char*      jmeth_name;
   char*      jclass_name;
   jobject    jobj;
   jlong      lseng;
   JExtPred*  pnext;

   ~JExtPred();
};
