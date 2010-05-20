#!/usr/bin/env python
# a small ssh server providing darcsden-specific darcs push/get.
# and that's all.
# 
# packages needed: twisted, pycrypto, pyasn1
#
# Once you have CouchDB running, you'll need a "root" user with
# private_key and public_key values. These will be used for the server.

import base64
import couchdb
import shlex
import sys

from twisted.cred import portal, checkers
from twisted.conch import error, avatar
from twisted.conch.checkers import SSHPublicKeyDatabase
from twisted.conch.interfaces import ISession
from twisted.conch.ssh import factory, userauth, connection, keys, session
from twisted.internet import reactor, protocol, defer
from twisted.python import components, log
from zope.interface import implements

log.startLogging(sys.stderr)


BASE = "/srv/darcs"
COUCH = couchdb.Server("http://localhost:4895/")
USERS = COUCH['users']
BY_NAME = USERS.view("users/by_name")


def sane(string):
    """
    Checks if the string uses a combination of whitelisted characters.

    Paranoid command security check.
    """
    return (string[0] != "/" and all(map(lambda x: x.isalnum() or x == '-' or x == '_' or x == '/', string)))


class DarcsUser(avatar.ConchUser):
    def __init__(self, username):
        avatar.ConchUser.__init__(self)
        self.name = username
        self.channelLookup.update({'session': session.SSHSession})


class DarcsRealm:
    implements(portal.IRealm)

    def requestAvatar(self, avatarId, mind, *interfaces):
        return interfaces[0], DarcsUser(avatarId), lambda: None


class DenyProtocol(protocol.Protocol):
    """
    A dumb protocol that just loses the connection once it's connected.

    Gets around bug #2754.
    """
    def connectionMade(self):
        self.transport.loseConnection()


class CouchDBChecker(SSHPublicKeyDatabase):
    def checkKey(self, credentials):
        try:
            keys = BY_NAME[credentials.username].rows[0].value['keys']

            for key in keys:
                l2 = key.split()
                if len(l2) < 2:
                    continue

                if base64.decodestring(l2[1]) == credentials.blob:
                    return True
        except Exception, e:
            print "exception %s occurred during pubkey check: %s" % (type(e), e)
            return False


class DarcsSession:
    implements(ISession)

    def __init__(self, avatar):
        self.user = avatar
        self.cmd = False

    def execCommand(self, proto, cmd):
        command = shlex.split(cmd)
        (type, info) = self.requestType(command)
        print "parsed requested command: %s, info: %s" % (type, info)

        if type == "transfer-mode":
            run = ("darcs", "transfer-mode", "--repodir", info[0])
        elif type == "apply":
            run = ("darcs", "apply", "--all", "--repodir", info[0])
        else:
            print "got something funny from %s: %s" % (self.user.name, cmd)
            self.reject(proto)
            return

        print "safe command: %s" % list(run)
        self.cmd = reactor.spawnProcess(proto, run[0], run)

    def reject(self, proto, msg = "GTFO"):
        proto.write("GTFO.\r\n")
        dp = DenyProtocol()
        dp.makeConnection(proto)
        proto.makeConnection(session.wrapProtocol(dp))

    def openShell(self, proto):
        self.reject(proto, "No shell for you.\r\n")

    def eofReceived(self):
        if self.cmd:
            self.cmd.closeStdin()

    def requestType(self, command):
        if command[0] != "darcs":
            return ("unknown", [])

        if  command[1] == "transfer-mode" \
        and command[2] == "--repodir" \
        and sane(command[3]) \
        and len(command) == 4:
            return ("transfer-mode", (self.absolute(command[3]),))

        if  command[1] == "apply" \
        and command[2] == "--all" \
        and command[3] == "--repodir" \
        and sane(command[4]) \
        and len(command) == 5:
            return ("apply", (self.absolute(command[4]),))

        return ("unknown", [])

    def absolute(self, path):
        return BASE + "/" + self.user.name + "/" + path

    def getPty(self, term, windowSize, attrs):
        pass

    def closed(self):
        self.cmd = False
        pass

components.registerAdapter(DarcsSession, DarcsUser, session.ISession)

class DarcsFactory(factory.SSHFactory):
    publicKeys = {
        'ssh-rsa': keys.Key.fromString(data=USERS['root']['public_key'])
    }
    privateKeys = {
        'ssh-rsa': keys.Key.fromString(data=USERS['root']['private_key'])
    }
    services = {
        'ssh-userauth': userauth.SSHUserAuthServer,
        'ssh-connection': connection.SSHConnection
    }
    

portal = portal.Portal(DarcsRealm())
portal.registerChecker(CouchDBChecker())
DarcsFactory.portal = portal

if __name__ == '__main__':
    reactor.listenTCP(22, DarcsFactory())
    reactor.run()
