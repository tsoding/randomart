#ifndef THREADS_H
#define THREADS_H

#ifdef _WIN32
    #include <windows.h>
    typedef struct {
        HANDLE thread;
    } threads_thread;
#else
    #include <pthread.h>
    typedef struct {
        pthread_t thread;
    } threads_thread;
#endif

int threads_create(threads_thread *thread, void *(*target)(void *), void *args);

#endif

#define THREADS_IMPLEMENTATION
#ifdef THREADS_IMPLEMENTATION

int threads_create(threads_thread *thread, void *(*target)(void *), void *args) {
    threads_thread t;
#ifdef _WIN32
    if (!(t.thread = CreateThread(NULL, 0, target, args, 0, NULL)))
        return 1;
#else
    if (pthread_create(&t.thread, NULL, target, args))
        return 1;
#endif
    if (thread)
        *thread = t;
    return 0;
}

int threads_join(threads_thread *thread, void **result) {
    void *value;
#ifdef _WIN32
    if (
        WaitForSingleObject(thread->thread, INFINITE) ||
        !GetExitCodeThread(thread->thread, &value)
    )
        return 1;
#else
    if (pthread_join(thread->thread, &value))
        return 1;
#endif
    if (result)
        *result = value;
    return 0;
}

#endif
