import SwiftUI
import SecureBlackbox

struct ShowMes: Identifiable {
    var id: String { message }
    
    let message: String
    var title: String = "Exception"
}

struct ContentView: View, OfficeDecryptorDelegate {
    var decryptor = OfficeDecryptor()
    var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
    
    @State private var inputFile: String = ""
    @State private var outputFile: String = ""
    @State private var decPass: String = ""
    @State private var docInfo: String = ""
    @State private var firstPassRequest = true
    @State private var showMessage: ShowMes?
        
    var body: some View {
        VStack(alignment: .center) {
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
            
            
            Text("Password for decryption:")
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
            SecureField("", text: $decPass)
                .frame(minWidth: 300, maxWidth: 300, alignment: .topLeading)
                
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
    private func decryptButton() -> some View {
        Button(action: {
            //decryptor.runtimeLicense = ""
            decryptor.delegate = self
            showMessage = nil
            docInfo = ""
            firstPassRequest = true
            decryptor.password = ""
            
            do {
                decryptor.inputFile = inputFile
                decryptor.outputFile = outputFile
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
    
    func onDecryptionPasswordNeeded(cancelDecryption: inout Bool) {
        if (!firstPassRequest) {
            cancelDecryption = true
            return
        }
        
        docInfo += "Document format: "
        switch (decryptor.documentFormat) {
            case OfficedecryptorDocumentFormats.odfBinary:
                docInfo += " Binary"
                break
            case OfficedecryptorDocumentFormats.odfOpenXML:
                docInfo += " OpenXML"
                break
            case OfficedecryptorDocumentFormats.odfOpenXPS:
                docInfo += " OpenXPS"
                break
            case OfficedecryptorDocumentFormats.odfOpenDocument:
                docInfo += " OpenDocument"
                break
            default:
                docInfo += " Unknown"
                break
        }
        docInfo += "\nEncryption type: "
        switch (decryptor.encryptionType) {
            case OfficedecryptorEncryptionTypes.oetBinaryRC4:
                docInfo += " Binary RC4"
                break
            case OfficedecryptorEncryptionTypes.oetBinaryRC4CryptoAPI:
                docInfo += " Binary RC4 CryptoAPI"
                break
            case OfficedecryptorEncryptionTypes.oetOpenXMLStandard:
                docInfo += " OpenXML Standard"
                break
            case OfficedecryptorEncryptionTypes.oetOpenXMLAgile:
                docInfo += " OpenXML Agile"
                break
            case OfficedecryptorEncryptionTypes.oetOpenDocument:
                docInfo += " OpenOffice Standard"
                break
            default:
                docInfo += " Unknown"
                break
        }
        docInfo += "\nEncryption algorithm: "
        docInfo += decryptor.encryptionAlgorithm
        if (decryptor.encryptionType == OfficedecryptorEncryptionTypes.oetBinaryRC4CryptoAPI) {
            docInfo += "\nKey length in bits: "
            do {
                docInfo += try decryptor.config(configurationString: "RC4KeyBits")
            } catch {
                docInfo += "Invalid value"
            }
            docInfo += "\nEncrypt Document Properties: "
            do {
                docInfo += try decryptor.config(configurationString: "EncryptDocumentProperties")
            } catch {
                docInfo += "Invalid value"
            }
            docInfo += "\nUse Hardened Key Generation: "
            do {
                docInfo += try decryptor.config(configurationString: "HardenedKeyGeneration")
            } catch {
                docInfo += "Invalid value"
            }
        } else if (decryptor.encryptionType == OfficedecryptorEncryptionTypes.oetOpenXMLAgile) {
            docInfo += "\nHash Algorithm: "
            do {
                docInfo += try decryptor.config(configurationString: "HashAlgorithm")
            } catch {
                docInfo += "Invalid value"
            }
            docInfo += "\nSalt Size: "
            do {
                docInfo += try decryptor.config(configurationString: "SaltSize")
            } catch {
                docInfo += "Invalid value"
            }
            docInfo += "\nSpin Count: "
            do {
                docInfo += try decryptor.config(configurationString: "SpinCount")
            } catch {
                docInfo += "Invalid value"
            }
        } else if (decryptor.encryptionType == OfficedecryptorEncryptionTypes.oetOpenDocument) {
            docInfo += "\nChecksum Algorithm: "
            do {
                docInfo += try decryptor.config(configurationString: "ChecksumAlgorithm")
            } catch {
                docInfo += "Invalid value"
            }
            docInfo += "\nStart Key Generation Algorithm: "
            do {
                docInfo += try decryptor.config(configurationString: "StartKeyGenerationAlgorithm")
            } catch {
                docInfo += "Invalid value"
            }
        }
        decryptor.password = decPass
        firstPassRequest = false
        cancelDecryption = false
    }
    
    func onError(errorCode: Int32, description: String) {}
    func onNotification(eventID: String, eventParam: String) {}
}

struct Contentview_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
