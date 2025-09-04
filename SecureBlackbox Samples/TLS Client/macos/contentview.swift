import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View, TLSClientDelegate {

    var client = TLSClient()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var serverHost: String = ""
    @State private var serverPort: String = "8080"
    @State private var useTLS = false
    @State private var sendText: String = ""
    @State private var messages: String = ""
    @State private var connected = false
    @State private var showMessage: ShowMes?
            
    var body: some View {
        VStack(alignment: .center) {
            Text("Server:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            TextField("", text: $serverHost)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            Text("Port:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            TextField("", text: $serverPort)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            Toggle(isOn: $useTLS) {
                Text("Use TLS")
            }
            
            .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                        
            HStack(alignment: .firstTextBaseline) {
                connectButton()
                disconnectButton()
            }
            
            
            Group {
                Text("Message:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                TextField("", text: $sendText)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                sendButton()
                Text("Output:")
                TextEditor(text: $messages)
                    .frame(minWidth: 300, maxWidth: 300, minHeight: 200, maxHeight: 300, alignment: .topLeading)
                    .border(Color.black, width: 1)
                    
            }
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
            messages = ""
            
            do {
                if (useTLS) {
                    client.tlsSettings.tlsMode = SSLModes.smDefault
                } else {
                    client.tlsSettings.tlsMode = SSLModes.smNoTLS
                }
                
                // for demo purposes only
                client.tlsSettings.autoValidateCertificates = false
                try client.connect(address: serverHost, port: Int32(serverPort) ?? 21)
                connected = true
            } catch {
                do {
                    try client.disconnect()
                }
                catch {}
                showMessage = ShowMes(message: "Failed to list mailboxes on the server. \(error)")
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
    private func sendButton() -> some View {
        Button(action: {
            do {
                try client.sendText(text: sendText)
                messages += "C->S: \(sendText) \n"
                
                try client.receiveAllData(size: Int32(sendText.count))
                messages += "S->C: \(client.outputString) \n"
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Send").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
        
        .disabled(connected == false)
    }
    
    func onCertificateValidate(address: String, accept: inout Bool) {
        accept = true
    }
    
    func onError(errorCode: Int32, description: String) {}
    func onExternalSign(operationId: String, hashAlgorithm: String, pars: String, data: String, signedData: inout String) {}
    func onNotification(eventID: String, eventParam: String) {}
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
