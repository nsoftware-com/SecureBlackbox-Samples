import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View, IMAPClientDelegate {

    var client = IMAPClient()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var connected = false
    @State private var host: String = ""
    @State private var port: String = "143"
    @State private var tlsType = 0
    @State private var login: String = ""
    @State private var password: String = ""
    @State private var selectedMailbox: Int = 0
    @State private var mesList: String = ""
    @State private var showMessage: ShowMes?
            
    private let mailboxPickerStyle = PopUpButtonPickerStyle()
    
    func mailboxName(num: Int) -> String {
        return client.mailboxes[num].name
    }
    
    func tlsTypeChange(_ tag: Int) {
        if (tag == 2) {
            if (port == "143"){
                port = "993"
            }
        } else {
            if (port == "993") {
                port = "143"
            }
        }
    }
    
    var body: some View {
        VStack(alignment: .center) {
            HStack(alignment: .top) {
                Text("Host:")
                    .frame(minWidth: 80, maxWidth: 80, alignment: .topLeading)
                    
                TextField("Enter server host", text: $host)
                    .frame(minWidth: 220, maxWidth: 220, alignment: .topLeading)
            }
            
            
            HStack(alignment: .firstTextBaseline) {
                Text("Port:")
                    .frame(minWidth: 80, maxWidth: 80, alignment: .topLeading)
                    
                TextField("Enter server port", text: $port)
                    .frame(minWidth: 220, maxWidth: 220, alignment: .topLeading)
            }
            HStack(alignment: .firstTextBaseline) {
                Text("Use TLS:")
                    .frame(minWidth: 80, maxWidth: 80, alignment: .topLeading)
                    
                Picker(selection: $tlsType, label: Text("")) {
                    Text("No")
                        .tag(0)
                    Text("Explicit")
                        .tag(1)
                    Text("Implicit")
                        .tag(2)
                }
                .pickerStyle(SegmentedPickerStyle())
                .frame(minWidth: 220, maxWidth: 220, alignment: .topLeading)
                .onChange(of: tlsType, perform: tlsTypeChange)
            }
            HStack(alignment: .firstTextBaseline) {
                Text("Login:")
                    .frame(minWidth: 80, maxWidth: 80, alignment: .topLeading)
                    
                TextField("Enter username", text: $login)
                    .frame(minWidth: 220, maxWidth: 220, alignment: .topLeading)
            }
            HStack(alignment: .firstTextBaseline) {
                Text("Password:")
                    .frame(minWidth: 80, maxWidth: 80, alignment: .topLeading)
                    
                SecureField("Enter password", text: $password)
                    .frame(minWidth: 220, maxWidth: 220, alignment: .topLeading)
            }
            HStack(alignment: .firstTextBaseline) {
                connectButton()
                disconnectButton()
            }
            
            
            Section {
                Picker("Mailboxes", selection: $selectedMailbox) {
                    let mailboxes = client.mailboxes;
                    ForEach ((0..<client.mailboxes.count), id: \.self) {
                        Text("\(mailboxes[$0].name)")
                    }
                }
                .pickerStyle(mailboxPickerStyle)
                .frame(minWidth: 300, maxWidth: 300, minHeight: 40, maxHeight: 40)
            }
            .frame(minWidth: 300, maxWidth: 300, minHeight: 40, maxHeight: 40)
            
            .disabled(connected == false)
                
            receiveButton()
            TextEditor(text: $mesList)
                .frame(minWidth: 300, maxWidth: 300, minHeight: 200, maxHeight: 300, alignment: .topLeading)
                .border(Color.black, width: 1)
        }
        .padding(.all, 8)
        .alert(item: $showMessage) { show in
            Alert(title: Text(show.title), message: Text(show.message), dismissButton: .default(Text("Ok"))) }
    }

    @ViewBuilder
    private func connectButton() -> some View {
        Button(action: {
            //client.runtimeLicense = ""
            client.delegate = self
            showMessage = nil

            do {
                switch (tlsType) {
                    case 1:
                        client.tlsSettings.tlsMode = SSLModes.smExplicitTLS
                        break
                    case 2:
                        client.tlsSettings.tlsMode = SSLModes.smImplicitTLS
                        break
                    default:
                        client.tlsSettings.tlsMode = SSLModes.smNoTLS
                        break
                }
  
                client.username = login
                client.password = password
                // for demo purposes only
                client.tlsSettings.autoValidateCertificates = false
                
                try client.connect(address: host, port: Int32(port) ?? 0);
            } catch {
                showMessage = ShowMes(message: "Failed to connect to the server. \(error)")
                do {
                    try client.disconnect();
                } catch {}
                return
            }

            showMessage = ShowMes(message: "Connected to the server. Listing mailboxes...", title: "Info")
            do {
                try client.listMailboxes()
                
                if (client.mailboxes.count == 0) {
                    showMessage = ShowMes(message: "No mailboxes on the server.", title: "Info")
                } else {
                    selectedMailbox = 0
                    connected = true
                }
            } catch {
                showMessage = ShowMes(message: "Failed to list mailboxes on the server. \(error)")
                do {
                    try client.disconnect()
                } catch {}
                return
            }
        }, label: {
            Text("Connect").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
        
        .disabled(connected == true)
    }
    
    @ViewBuilder
    private func disconnectButton() -> some View {
        Button(action: {
            showMessage = nil
            do {
                try client.disconnect()
                connected = false
                mesList = ""
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Disconnect").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
        
        .disabled(connected == false)
    }
    
    @ViewBuilder
    private func receiveButton() -> some View {
        Button(action: {
            showMessage = nil
            mesList = ""
            
            do {
                try client.selectMailbox(name: client.mailboxes[selectedMailbox].name)
                
                mesList += "Name: \(client.currentMailbox.name) \n"
                mesList += "Total Messages: \(client.currentMailbox.totalMessages) \n"
                mesList += "Unseen Messages: \(client.currentMailbox.unseenMessages) \n"
                mesList += "Recent Messages: \(client.currentMailbox.recentMessages) \n"
                
                if (client.currentMailbox.unseenMessages > 0) {
                    mesList += "\nList of unseen messages: \n"
                    try client.listUnseenMessages()
                    loadMessages()
                } else {
                    mesList += "\nList of all messages: \n"
                    try client.listAllMessages()
                    loadMessages()
                }
            } catch {
                showMessage = ShowMes(message: "Failed to select the mailbox. \(error)")
                return
            }
        }, label: {
            Text("Receive").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
        
        .disabled(connected == false)
    }

    func loadMessages() {
        var i = 0;
        for message in client.messages
        {
            i += 1;
            mesList += "    Message# \(i) \n";
            mesList += "        From: \(message.from) \n";
            mesList += "        To: \(message.sentTo) \n";
            mesList += "        Subject: \(message.subject) \n";
            mesList += "        Date: \(message.date) \n";
            mesList += "        Size: \(message.size) \n";
        }
    }

    func onBeforeAuth() {}
    
    func onCertificateValidate(address: String, accept: inout Bool)
    {
        // for demo purposes only
        accept = true;
    }
    
    func onCommand(cmd: String) {}
    func onCommandData(cmd: String, data: String) {}
    func onCommandReply(cmd: String, reply: String) {}
    func onCommandReplyData(cmd: String, data: String) {}
    func onError(errorCode: Int32, description: String) {}
    func onMailboxStatus(name: String, totalMessages: Int32, recentMessages: Int32, unseenMessages: Int32, nextUID: Int32, uidValidity: Int32) {}
    func onNotification(eventID: String, eventParam: String) {}
    func onProgress(total: Int64, current: Int64, cancel: inout Bool) {}
    func onTLSCertNeeded(host: String, caNames: String) {}
    func onTLSCertValidate(serverHost: String, serverIP: String, accept: inout Bool) {}
    func onTLSEstablished(host: String, version: String, ciphersuite: String, connectionId: Data, abort: inout Bool) {}
    func onTLSHandshake(host: String, abort: inout Bool) {}
    func onTLSPSK(host: String, hint: String) {}
    func onTLSShutdown(host: String) {}
}

struct Contentview_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
