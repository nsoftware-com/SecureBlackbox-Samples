import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View, PDFDecryptorDelegate {
    var decryptor = PDFDecryptor()
    var certmanager = CertificateManager()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var inputFile: String = ""
    @State private var outputFile: String = ""
    @State private var certFile: String = ""
    @State private var certPass: String = ""
    @State private var decPass: String = ""
    @State private var docInfo: String = ""
    @State private var firsPassRequest = true
    @State private var showMessage: ShowMes?
        
    var body: some View {
        VStack(alignment: .center) {
            Text("Input file:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
            TextField("", text: $inputFile)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                .autocapitalization(.none)
            }
            
            
            Text("Output file:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            HStack(alignment: .firstTextBaseline) {
            TextField("", text: $outputFile)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            }
            
            
            Group {
                Text("Provide password for decryption:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                SecureField("", text: $decPass)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                    .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
                Text("Provide certificate file for decryption:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                HStack(alignment: .firstTextBaseline) {
                TextField("", text: $certFile)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
                }
                
                
                Text("Certificate password:")
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                SecureField("", text: $certPass)
                    .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                    
                    .autocapitalization(/*@START_MENU_TOKEN@*/.none/*@END_MENU_TOKEN@*/)
            }
            decryptButton()
            TextEditor(text: $docInfo)
                .frame(minWidth: 300, maxWidth: 300, minHeight: 200, maxHeight: 300, alignment: .topLeading)
                .border(Color.black, width: 1)
                
        }
        .padding(.all, 8)
        .alert(item: $showMessage) { show in
            Alert(title: Text(show.title), message: Text(show.message), dismissButton: .default(Text("Ok"))) }
    }

    @ViewBuilder
    private func decryptButton() -> some View {
        Button(action: {
            //decryptor.runtimeLicense = ""
            decryptor.delegate = self
            showMessage = nil
            docInfo = ""
            firsPassRequest = true
            decryptor.password = ""
             
            do {
                decryptor.inputFile = documentsPath + inputFile
                decryptor.outputFile = documentsPath + outputFile
                try decryptor.decrypt()
                showMessage = ShowMes(message: "The file successfully decrypted", title: "Info")
            } catch {
                showMessage = ShowMes(message: "Error \(error)")
                return
            }
        }, label: {
            Text("Decrypt").font(.system(size: 20)).frame(minWidth: 150, minHeight: 40).background(RoundedRectangle(cornerRadius: 8).fill(Color.gray))
        })
        .buttonStyle(PlainButtonStyle())
    }
    
    func onDecryptionInfoNeeded(cancelDecryption: inout Bool) {
        if (!firsPassRequest) {
            cancelDecryption = true
            return
        }
        
        if (decryptor.documentInfo.encryptionType == PDFEncryptionTypes.petPassword) {
            docInfo += "Encryption type: password"
            decryptor.password = decPass
            cancelDecryption = false
        } else {
            docInfo += "Encryption type: certificate"
            if (certFile != "") {
                do {
                    try certmanager.importFromFile(path: documentsPath + certFile, password: certPass)
                    decryptor.decryptionCertificate = certmanager.certificate
                    cancelDecryption = false
                } catch {
                    showMessage = ShowMes(message: "Error \(error)")
                    cancelDecryption = true
                    return
                }
            } else {
                showMessage = ShowMes(message: "Certificate for decryption not set")
                cancelDecryption = true
                return
            }
        }
        
        docInfo += "\nEncryption algorithm: "
        docInfo += decryptor.documentInfo.encryptionAlgorithm
        if (decryptor.documentInfo.metadataEncrypted) {
            docInfo += "\nMetadata status: encrypted"
        } else {
            docInfo += "\nMetadata status: not encrypted"
        }
        
        firsPassRequest = false
    }
    
    func onError(errorCode: Int32, description: String) {}
    func onExternalDecrypt(operationId: String, algorithm: String, pars: String, encryptedData: String, data: inout String) {}
    func onNotification(eventID: String, eventParam: String) {}
    func onRecipientFound(issuerRDN: String, serialNumber: Data, subjectKeyID: Data, certFound: Bool) {}
    func onDocumentLoaded(cancel: inout Bool) {}
    func onEncrypted(certUsed: Bool, issuerRDN: String, serialNumber: Data, subjectKeyID: Data, needCredential: Bool, skipThis: inout Bool) {}
}

struct Contentview_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
