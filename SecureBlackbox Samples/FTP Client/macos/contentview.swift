import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View, FTPClientDelegate {

    var client = FTPClient()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var server: String = ""
    @State private var username: String = ""
    @State private var password: String = ""
    @State private var filename: String = ""
    @State private var outputRes: String = ""
    @State private var connected = false
    @State private var showMessage: ShowMes?
        
    func connectedChange() -> String {
        if (connected) {
            return "Disconnect"
        } else {
            return "Connect"
        }
    }
    
    var body: some View {
        VStack(alignment: .center) {
            Text("Server:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            TextField("", text: $server)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            Text("Username:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            TextField("", text: $username)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            Text("Password:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            SecureField("", text: $password)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            connectButton()
            Group {
                Text("Filename:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                HStack(alignment: .firstTextBaseline) {
                TextField("", text: $filename)
                    .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
                openFileButton()
                }
                
                
                HStack() {
                    getButton()
                    putButton()
                }
                
                Text("Output:")
                TextEditor(text: $outputRes)
                    .frame(minWidth: 300, maxWidth: 300, minHeight: 200, maxHeight: 300, alignment: .topLeading)
                    .border(Color.black, width: 1)
            }
        }
        .padding(.all, 8)
        .alert(item: $showMessage) { show in
            Alert(title: Text(show.title), message: Text(show.message), dismissButton: .default(Text("Ok"))) }
    }
   
    @ViewBuilder
    private func openFileButton() -> some View {
        Button(action: {
            let dialog = NSOpenPanel()
            dialog.title = "Choose an file"
            dialog.showsResizeIndicator = true
            dialog.showsHiddenFiles = false
            dialog.allowsMultipleSelection = false
            dialog.canChooseDirectories = false
                
            if (dialog.runModal() ==  NSApplication.ModalResponse.OK) {
                let result = dialog.url
                if (result != nil) {
                    filename = result!.path
                }
            }
        }, label: {
            Text("...").font(.system(size: 18))
                .frame(minWidth: 20, minHeight: 20)
                .background(RoundedRectangle(cornerRadius: 2).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
    
    @ViewBuilder
    private func connectButton() -> some View {
        Button(action: {
            //client.runtimeLicense = ""
            client.delegate = self
            showMessage = nil
            outputRes = ""
            
            do {
                if (client.connected == true) {
                    try client.disconnect()
                } else {
                    client.username = username
                    client.password = password
                    
                    try client.connect(address: server, port: 21)
                    try client.listDir(includeFiles: true, includeDirectories: true)
                }
                
                connected = client.connected
            } catch {
                do {
                    try client.disconnect()
                }
                catch {}
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("\(connectedChange())").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
    
    @ViewBuilder
    private func getButton() -> some View {
        Button(action: {
            do {
                let dialog = NSSavePanel()
                dialog.title = "Enter where to save resulting file"
                dialog.showsResizeIndicator = true
                dialog.showsHiddenFiles = false

                if (dialog.runModal() ==  NSApplication.ModalResponse.OK) {
                    if (dialog.url != nil) {
                        try client.downloadFile(remoteFile: filename, localFile: dialog.url!.path)
                        showMessage = ShowMes(message: "File successfully downloaded")
                    }
                }
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Get").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle()).disabled(connected == false)
    }
    
    @ViewBuilder
    private func putButton() -> some View {
        Button(action: {
            outputRes = ""
            do {
                let filePath = filename
                try client.uploadFile(localFile: filePath, remoteFile: (filePath as NSString).lastPathComponent)
                try _ = client.listDir(includeFiles: true, includeDirectories: true)
                showMessage = ShowMes(message: "File successfully uploaded")
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Put").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle()).disabled(connected == false)
    }

    func onCertificateValidate(address: String, accept: inout Bool) {}
    func onControlReceive(textLine: String) {}
    func onControlSend(textLine: String) {}
    func onError(errorCode: Int32, description: String) {}
    func onExternalSign(operationId: String, hashAlgorithm: String, pars: String, data: String, signedData: inout String) {}
    func onFileOperation(operation: Int32, remotePath: String, localPath: String, skip: inout Bool, cancel: inout Bool) {}
    func onFileOperationResult(operation: Int32, remotePath: String, localPath: String, errorCode: Int32, comment: String, cancel: inout Bool) {}
    
    func onListEntry(filename: String) {
        outputRes += filename + "\n"
    }
    
    func onNotification(eventID: String, eventParam: String) {}
    func onProgress(total: Int64, current: Int64, cancel: inout Bool) {}
    func onTextDataLine(textLine: String) {}
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
