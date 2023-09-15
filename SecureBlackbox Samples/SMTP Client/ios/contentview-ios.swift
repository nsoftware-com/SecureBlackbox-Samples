import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View, SMTPClientDelegate {

    var client = SMTPClient()
    var writer = MailWriter()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var host: String = ""
    @State private var port: String = "25"
    @State private var tlsType = 0
    @State private var login: String = ""
    @State private var password: String = ""
    @State private var from: String = ""
    @State private var to: String = ""
    @State private var subject: String = ""
    @State private var plainText: String = ""
    @State private var showMessage: ShowMes?
    
    func tlsTypeChange(_ tag: Int) {
        if (tag == 2) {
            if (port == "25" || port == "587") {
                port = "465"
            }
        } else if (tag == 1) {
            if (port == "25" || port == "465") {
                port = "587"
            }
        } else {
            if (port == "465" || port == "587") {
                port = "25"
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
            Group {
                HStack(alignment: .top) {
                    Text("From:")
                        .frame(minWidth: 80, maxWidth: 80, alignment: .topLeading)
                        
                    TextField("Enter 'from' address", text: $from)
                        .frame(minWidth: 220, maxWidth: 220, alignment: .topLeading)
                        .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
                }
                HStack(alignment: .firstTextBaseline)
                {
                    Text("To:")
                        .frame(minWidth: 80, maxWidth: 80, alignment: .topLeading)
                        
                    TextField("Enter 'to' address", text: $to)
                        .frame(minWidth: 220, maxWidth: 220, alignment: .topLeading)
                        .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
                }
                HStack(alignment: .firstTextBaseline)
                {
                    Text("Subject:")
                        .frame(minWidth: 80, maxWidth: 80, alignment: .topLeading)
                        
                    TextField("Enter subject", text: $subject)
                        .frame(minWidth: 220, maxWidth: 220, alignment: .topLeading)
                        .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
                }
                Text("Plain text:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                TextEditor(text: $plainText)
                    .frame(minWidth: 300, maxWidth: 300, minHeight: 200, maxHeight: 300, alignment: .topLeading)
                    .border(Color.black, width: 1)
                    
            }
            sendButton()
        }
        .padding(.all, 8)
        .alert(item: $showMessage) { show in
            Alert(title: Text(show.title), message: Text(show.message), dismissButton: .default(Text("Ok"))) }
    }

    @ViewBuilder
    private func sendButton() -> some View {
        Button(action: {
            //client.runtimeLicense = ""
            client.delegate = self
            showMessage = nil
            do {
                writer.from.append(MailAddress(displayName: "from name", address: from))
                client.message.from = writer.message.from
                writer.sendTo.append(MailAddress(displayName: "to name", address: to))
                client.message.sendTo = writer.message.sendTo
                client.message.subject = subject
                client.message.plainText = plainText
                        
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
                client.domain = "testdomain"
                
                try client.connect(address: host, port: Int32(port) ?? 0)
                try client.sendMessage()
                try client.disconnect()
                showMessage = ShowMes(message: "Message has been sent successfully", title: "Info");
            } catch {
                showMessage = ShowMes(message: "Failed to send the message. \(error)")
                do {
                    try client.disconnect()
                } catch {}
                return
            }
        }, label: {
            Text("Send").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
    
    func onBeforeAuth() {}
    
    func onCertificateValidate(address: String, accept: inout Bool) {
        // for demo purposes only
        accept = true
    }
    
    func onCommand(cmd: String) {}
    func onCommandData(cmd: String, data: String) {}
    func onCommandReply(cmd: String, reply: String) {}
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
