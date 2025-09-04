import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View {
    var signer = MessageSigner()
    var certmanager = CertificateManager()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var signType = 0
    @State private var inputFile: String = ""
    @State private var outputFile: String = ""
    @State private var certFile: String = ""
    @State private var certPass: String = ""
    @State private var showMessage: ShowMes?
        
    var body: some View {
        VStack(alignment: .center) {
            Text("Signature type:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            Picker(selection: $signType, label: Text("")) {
                Text("PKCS7 Detached")
                    .tag(0)
                Text("PKCS7 Enveloping")
                    .tag(1)
            }
            .pickerStyle(SegmentedPickerStyle())
            .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
            
            Group {
                Text("Input file:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                HStack(alignment: .firstTextBaseline) {
                TextField("", text: $inputFile)
                    .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
                openInputButton()
                }
                
                
                Text("Output file:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                HStack(alignment: .firstTextBaseline) {
                TextField("", text: $outputFile)
                    .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
                saveOutputButton()
                }
                
            }
            Text("Certificate file:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
            TextField("", text: $certFile)
                .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
            openCertButton()
            }
            
            
            Text("Password:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            SecureField("", text: $certPass)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            signButton()
        }
        .padding(.all, 8)
        .alert(item: $showMessage) { show in
            Alert(title: Text(show.title), message: Text(show.message), dismissButton: .default(Text("Ok"))) }
    }

    @ViewBuilder
    private func openInputButton() -> some View {
        Button(action: {
            let dialog = NSOpenPanel()
            dialog.title = "Choose an input file"
            dialog.showsResizeIndicator = true
            dialog.showsHiddenFiles = false
            dialog.allowsMultipleSelection = false
            dialog.canChooseDirectories = false

            if (dialog.runModal() ==  NSApplication.ModalResponse.OK) {
                let result = dialog.url
                if (result != nil) {
                    inputFile = result!.path
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
    private func saveOutputButton() -> some View {
        Button(action: {
            let dialog = NSSavePanel()
            dialog.title = "Choose an output file"
            dialog.showsResizeIndicator = true
            dialog.showsHiddenFiles = false

            if (dialog.runModal() ==  NSApplication.ModalResponse.OK) {
                let result = dialog.url
                if (result != nil) {
                    outputFile = result!.path
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
    private func openCertButton() -> some View {
        Button(action: {
            let dialog = NSOpenPanel()
            dialog.title                   = "Choose an certificate file"
            dialog.showsResizeIndicator    = true
            dialog.showsHiddenFiles        = false
            dialog.allowsMultipleSelection = false
            dialog.canChooseDirectories = false
            dialog.allowedFileTypes        = ["cer", "pem", "pfx"]

            if (dialog.runModal() ==  NSApplication.ModalResponse.OK) {
                let result = dialog.url
                if (result != nil) {
                    certFile = result!.path
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
    private func signButton() -> some View {
        Button(action: {
            //signer.runtimeLicense = ""
            showMessage = nil
            
            if (signType == 0) {
                signer.signatureType =  MessagesignerSignatureTypes.stPKCS7Detached
            } else {
                signer.signatureType = MessagesignerSignatureTypes.stPKCS7Enveloping
            }
            signer.hashAlgorithm = "SHA256"
            signer.inputFile = inputFile
            signer.outputFile = outputFile
            let certPath = certFile
            do {
                try certmanager.importFromFile(path: certPath, password: certPass)
                signer.signingCertificate = certmanager.certificate

                try signer.sign()
                showMessage = ShowMes(message: "The file successfully signed", title: "Info")
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Sign").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
}

struct Contentview_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
