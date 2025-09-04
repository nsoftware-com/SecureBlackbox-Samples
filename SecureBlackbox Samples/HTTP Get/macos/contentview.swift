import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}
struct ContentView: View, HTTPClientDelegate {

    var client = HTTPClient()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var url: String = "https://www.google.com"
    @State private var output: String = ""
    @State private var showMessage: ShowMes?
        
    var body: some View {
        VStack(alignment: .center) {
            Text("URL:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            TextField("Enter url", text: $url)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            getButton()
            Text("Output:")
            TextEditor(text: $output)
                .frame(minWidth: 300, maxWidth: 300, minHeight: 200, maxHeight: 300, alignment: .topLeading)
                .border(Color.black, width: 1)
                
        }
        .padding(.all, 8)
        .alert(item: $showMessage) { show in
            Alert(title: Text(show.title), message: Text(show.message), dismissButton: .default(Text("Ok"))) }
    }
    
    @ViewBuilder
    private func getButton() -> some View {
        Button(action: {
            //client.runtimeLicense = ""
            client.delegate = self
            showMessage = nil
            output = ""
            
            do {
                try client.get(url: url)
                output += client.outputString
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Get").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
    
    func onCertificateValidate(address: String, accept: inout Bool) {
        accept = true
    }
        
    func onCookie(cookieText: String) {}
        
    func onDocumentBegin() {
            output += "--- Document started ---\n\n"
    }
        
    func onDocumentEnd() {
            output += "--- Document finished ---\n\n"
    }
        
    func onDynamicDataNeeded(bytesNeeded: Int32) {}
        
    func onError(errorCode: Int32, description: String) {}
        
    func onExternalSign(operationId: String, hashAlgorithm: String, pars: String, data: String, signedData: inout String) {}
        
    func onHeadersPrepared() {
        output += "Headers sent: \n"
        for header in client.requestHeaders {
            output += "\(header.name): \(header.value) \n"
        }
        output += "\n"
    }
        
    func onHeadersReceived() {
        output += "Headers received: \n"
        for header in client.responseHeaders {
            output += "\(header.name): \(header.value) \n"
        }
        output += "\n"
    }
        
    func onNotification(eventID: String, eventParam: String) {}
    func onProgress(total: Int64, current: Int64, cancel: inout Bool) {}
        
    func onRedirection(oldURL: String, newURL: inout String, allowRedirection: inout Bool) {
        output += "Request redirected to \(newURL) \n"
        allowRedirection = true
    }

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
