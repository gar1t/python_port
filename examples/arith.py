from erlport import Port, Protocol

class Arith(Protocol):

    def handle_add(self, x, y):
        return x + y

    def handle_subtr(self, x, y):
        return x - y

    def handle_mult(self, x, y):
        return x * y

    def handle_div(self, x, y):
        return x / y

if __name__ == '__main__':
    Arith().run(Port(packet=4, use_stdio=True))
