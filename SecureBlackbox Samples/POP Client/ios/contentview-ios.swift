import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View, POP3ClientDelegate {

    var client = POP3Client()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var connected = false
    @State private var host: String = ""
    @State private var port: String = "110"
    @State private var tlsType = 0
    @State private var login: String = ""
    @State private var password: String = ""
    @State private var selectedMessages: Int32 = 0
    @State private var mesInfo: String = ""
    @State private var showMessage: ShowMes?
            
    private let mailboxPickerStyle = WheelPickerStyle()
    
    func msgName(num: Int) -> String {
        return client.messages[num].uid
    }
    
    func tlsTypeChange(_ tag: Int) {
        if (tag == 2) {
            if (port == "110") {
                port = "995"
            }
        } else {
            if (port == "995") {
                port = "110"
            }
        }
    }
    
    var body: some View {
        VStack(alignment: .center) {
            HStack(alignment: .top)
            {
                Text("Host:")
                    .frame(minWidth: 80, maxWidth: 80, alignment: .topLeading)
                TextField("Enter server host", text: $host)
                    .frame(minWidth: 220, maxWidth: 220, alignment: .topLeading)
                    .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            }
            
            
            HStack(alignment: .firstTextBaseline)
            {
                Text("Port:")
                    .frame(minWidth: 80, maxWidth: 80, alignment: .topLeading)
                    
                TextField("Enter server port", text: $port)
                    .frame(minWidth: 220, maxWidth: 220, alignment: .topLeading)
                    .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            }
            HStack(alignment: .firstTextBaseline)
            {
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
            HStack(alignment: .firstTextBaseline)
            {
                Text("Login:")
                    .frame(minWidth: 80, maxWidth: 80, alignment: .topLeading)
                    
                TextField("Enter username", text: $login)
                    .frame(minWidth: 220, maxWidth: 220, alignment: .topLeading)
                    .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            }
            HStack(alignment: .firstTextBaseline)
            {
                Text("Password:")
                    .frame(minWidth: 80, maxWidth: 80, alignment: .topLeading)
                    
                SecureField("Enter password", text: $password)
                    .frame(minWidth: 220, maxWidth: 220, alignment: .topLeading)
                    .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)

            }
            HStack(alignment: .firstTextBaseline) {
                connectButton()
                disconnectButton()
            }
            
            
            Section {
                Picker("Mailboxes", selection: $selectedMessages) {
                    ForEach((0..<client.messages.count), id: \.self) { Text("\(msgName(num: $0))") }
                }
                .pickerStyle(mailboxPickerStyle)
                .frame(minWidth: 300, maxWidth: 300, minHeight: 40, maxHeight: 40)
            }
            .frame(minWidth: 300, maxWidth: 300, minHeight: 40, maxHeight: 40)
            
            .disabled(connected == false)
                
            receiveButton()
            TextEditor(text: $mesInfo)
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
                try _ = client.config(configurationString: "RequestUIDs=True")
                
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
                try client.connect(address: host, port: Int32(port) ?? 0)
            } catch {
                showMessage = ShowMes(message: "Failed to connect to the server. \(error)")
                do {
                    try client.disconnect();
                } catch {}
                return
            }
            
            showMessage = ShowMes(message: "Connected to the server. Listing messages...", title: "Info")
            do {
                try client.listMessages()
                
                if (client.messages.count == 0) {
                    showMessage = ShowMes(message: "No messages on the server.", title: "Info")
                } else {
                    selectedMessages = 0
                    connected = true
                }
            } catch {
                showMessage = ShowMes(message: "Failed to list mailboxes on the server. \(error)")
                do {
                    try client.disconnect();
                } catch {}
                return
            }
        }, label: {
            Text("Connect").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle()).disabled(connected == true)
    }
    
    @ViewBuilder
    private func disconnectButton() -> some View {
        Button(action: {
            showMessage = nil
            do {
                try client.disconnect()
                connected = false
                mesInfo = ""
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Disconnect").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle()).disabled(connected == false)
    }
    
    @ViewBuilder
    private func receiveButton() -> some View {
        Button(action: {
            showMessage = nil
            mesInfo = ""
            do {
                try client.receiveMessage(index: selectedMessages)
                
                mesInfo += "From: \(client.message.from) \n"
                mesInfo += "To: \(client.message.sendTo) \n"
                mesInfo += "Date: \(client.message.date) \n"
                mesInfo += "Subject: \(client.message.subject) \n"
                if (client.message.attachmentCount > 0) {
                    mesInfo += "Attachments: \(client.message.attachmentCount) \n"
                } else {
                    mesInfo += "Attachments: [none] \n"
                }
                mesInfo += "Text: \(client.message.from)"
                mesInfo += client.message.plainText
            } catch {
                showMessage = ShowMes(message: "Failed to receive the message. \(error)")
                return
            }
        }, label: {
            Text("Receive").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle()).disabled(connected == false)
    }
    
    func onBeforeAuth() {}
    
    func onCertificateValidate(address: String, accept: inout Bool) {
        // for demo purposes only
        accept = true
    }

    func onCommand(cmd: String) {}
    func onCommandReply(cmd: String, reply: String) {}
    func onCommandReplyData(cmd: String, data: String) {}
    func onError(errorCode: Int32, description: String) {}
    func onExternalSign(operationId: String, hashAlgorithm: String, pars: String, data: String, signedData: inout String) {}
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
