package scalaprops;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.util.Arrays;

final class ThreadIdCompat {
    private static final MethodHandle threadIdMethodHandle;

    static {
        final String methodName;
        if (Arrays.stream(Thread.class.getMethods())
                .filter(x -> "threadId".equals(x.getName()))
                .findFirst()
                .isPresent()) {
            methodName = "threadId";
        } else {
            methodName = "getId";
        }

        try {
            threadIdMethodHandle =
                    MethodHandles.lookup()
                            .findVirtual(Thread.class, methodName, MethodType.methodType(Long.TYPE));
        } catch (NoSuchMethodException | IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }

    static long threadId() throws Throwable {
        return (long) threadIdMethodHandle.invoke(Thread.currentThread());
    }
}
