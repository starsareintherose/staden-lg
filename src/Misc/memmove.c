/*
 * memmove() does not exist on SunOS, despite being an ANSI library call.
 */
void *memmove(void *to, const void *from, size_t len) {
    bcopy(from, to, len);
    return to;
}
