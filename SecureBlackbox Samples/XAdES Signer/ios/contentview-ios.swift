import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View {
    var signer = XAdESSigner()
    var certmanager = CertificateManager()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var inputFile: String = ""
    @State private var outputFile: String = ""
    @State private var detached = false
    @State private var version = 0
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
            
            Text("XAdES version:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            Picker(selection: $version, label: Text("")) {
                Text("No")
                    .tag(0)
                Text("1.1.1")
                    .tag(1)
                Text("1.2.2")
                    .tag(2)
                Text("1.3.2")
                    .tag(3)
                Text("1.4.1")
                    .tag(4)
            }
            .pickerStyle(SegmentedPickerStyle())
            
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
            
                if (detached) {
                    signer.dataFile = documentsPath + inputFile
                    signer.dataType = XadessignerDataTypes.cxdtBinary
                    signer.dataURI = (inputFile as NSString).lastPathComponent
                    signer.newSignature.signatureType = XMLSignatureTypes.cxstDetached
                } else {
                    signer.inputFile = documentsPath + inputFile
                    signer.newSignature.signatureType = XMLSignatureTypes.cxstEnveloped
                }
                
                switch (version) {
                    case 1:
                    signer.newSignature.xAdESVersion = XAdESVersions.xav111
                    signer.newSignature.level = AdESSignatureLevels.aslBES
                        break;
                    case 2:
                    signer.newSignature.xAdESVersion = XAdESVersions.xav122
                    signer.newSignature.level = AdESSignatureLevels.aslBES
                        break;
                    case 3:
                    signer.newSignature.xAdESVersion = XAdESVersions.xav132;
                    signer.newSignature.level = AdESSignatureLevels.aslBES;
                        break
                    case 4:
                    signer.newSignature.xAdESVersion = XAdESVersions.xav141
                    signer.newSignature.level = AdESSignatureLevels.aslBES
                        break
                    default:
                    signer.newSignature.level = AdESSignatureLevels.aslGeneric
                        break
                }
                
                signer.newSignature.canonicalizationMethod = XMLCanonicalizationMethods.cxcmCanon
                signer.newSignature.hashAlgorithm = "SHA256"
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
