import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View {
    var signer = XMLSigner()
    var certmanager = CertificateManager()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var inputFile: String = ""
    @State private var outputFile: String = ""
    @State private var detached = false
    @State private var certFile: String = ""
    @State private var certPass: String = ""
    @State private var showMessage: ShowMes?
        
    var body: some View {
        VStack(alignment: .center) {
            Text("Input file:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
            TextField("", text: $inputFile)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            }
            
            
            Text("Output file:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
            TextField("", text: $outputFile)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            }
            
            
            Toggle(isOn: $detached) {
                Text("Detached")
            }
            
            .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
            
            Group {
                Text("Certificate file:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                HStack(alignment: .firstTextBaseline) {
                TextField("", text: $certFile)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
                }
                
                
                Text("Password:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                SecureField("", text: $certPass)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                    .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            }
            signButton()
        }
        .padding(.all, 8)
        .alert(item: $showMessage) { show in
            Alert(title: Text(show.title), message: Text(show.message), dismissButton: .default(Text("Ok"))) }
    }

    @ViewBuilder
    private func signButton() -> some View {
        Button(action: {
            //signer.runtimeLicense = ""
            showMessage = nil
            signer.outputFile = documentsPath + outputFile
            let certPath = documentsPath + certFile
    do {
                try certmanager.importFromFile(path: certPath, password: certPass)
                signer.signingCertificate = certmanager.certificate
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
            
            do {
                if (detached) {
                    signer.dataFile = documentsPath + inputFile
                    signer.dataType = XmlsignerDataTypes.cxdtBinary
                    signer.dataURI = (inputFile as NSString).lastPathComponent

                    signer.signatureType = XmlsignerSignatureTypes.cxstDetached
                } else {
            signer.inputFile = documentsPath + inputFile
                    signer.signatureType = XmlsignerSignatureTypes.cxstEnveloped
                }
                
                signer.canonicalizationMethod = XmlsignerCanonicalizationMethods.cxcmCanon
                signer.hashAlgorithm = "SHA256"
                try _ = signer.config(configurationString: "XMLFormatting=auto")
                
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
