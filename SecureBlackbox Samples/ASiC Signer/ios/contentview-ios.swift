import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}
struct ContentView: View {
    var signer = ASiCSigner()
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
            Group {
                Text("Signature type:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                Picker(selection: $signType, label: Text("")) {
                    Text("CAdES")
                        .tag(0)
                    Text("XAdES")
                        .tag(1)
                }
                .pickerStyle(SegmentedPickerStyle())
                
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
            }
            
            Text("Input file:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
#if os(iOS)
            TextField("", text: $inputFile)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
#else
            TextField("", text: $inputFile)
                .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
            openInputButton()
#endif
            }
            
            
            Text("Output file:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
#if os(iOS)
            TextField("", text: $outputFile)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
#else
            TextField("", text: $outputFile)
                .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
            saveOutputButton()
#endif
            }
            
                
            Text("Certificate file:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
#if os(iOS)
            TextField("", text: $certFile)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
#else
            TextField("", text: $certFile)
                .frame(minWidth: 270, maxWidth: 270, alignment: .topLeading)
            openCertButton()
#endif
            }
            
                
            Text("Password:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
#if os(iOS)
            SecureField("", text: $certPass)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
#else
            SecureField("", text: $certPass)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
#endif
            signButton()
        }
        .padding(.all, 8)
        .alert(item: $showMessage) { show in
            Alert(title: Text(show.title), message: Text(show.message), dismissButton: .default(Text("Ok"))) }
    }

#if !os(iOS)
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
#endif
    
#if !os(iOS)
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
#endif
    
#if !os(iOS)
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
#endif
    
    @ViewBuilder
    private func signButton() -> some View {
        Button(action: {
            //signer.runtimeLicense = ""
            showMessage = nil
            
            if (signType == 1) {
                signer.newSignature.signatureType = ASiCSignatureTypes.castXAdES
            } else {
                signer.newSignature.signatureType = ASiCSignatureTypes.castCAdES
            }
                    
            signer.newSignature.level = AdESSignatureLevels.aslBES
            signer.extended = false
            signer.ignoreChainValidationErrors = true
#if os(iOS)
            signer.sourceFiles = documentsPath + inputFile
            signer.outputFile = documentsPath + outputFile
            let certPath = documentsPath + certFile
#else
            signer.sourceFiles = inputFile
            signer.outputFile = outputFile
            let certPath = certFile
#endif
            
            do {
                try certmanager.importFromFile(path: certPath, password: certPass)
                signer.signingCertificate = certmanager.certificate
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
            
            do {
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
