from erlport import Port, Protocol

class Echo(Protocol):

    def handle_echo(self, msg):
        return msg

if __name__ == '__main__':
    Echo().run(Port(packet=4, use_stdio=True))
