//== ErrorMessages
// Class with error messages string constants
function ErrorMessages() {}

ErrorMessages.InvalidRequest = "Invalid request JSON string";
ErrorMessages.InvalidValueType = "Invalid value type";

//== JSONConstants
// Class with JSON string constants
function JSONConstants() {}

JSONConstants.RootName = "SecureBlackboxAsyncState";
JSONConstants.TypeName = "Type";
JSONConstants.SubtypesName = "Subtypes";
JSONConstants.SubtypeName = "Subtype";
JSONConstants.PKCS7SubtypeName = "pkcs7sig";
JSONConstants.RawSubtypeName = "pkcs1sig";
JSONConstants.GeneratorName = "Generator";
JSONConstants.MsgName = "RootMessage";
JSONConstants.MsgTypeName = "MessageType";
JSONConstants.MsgIdName = "MessageID";
JSONConstants.MsgNameName = "Name";
JSONConstants.MsgParamsName = "Pars";
JSONConstants.MsgParamName = "Par";
JSONConstants.RootMsgTypeName = "Message.Batch";
JSONConstants.BatchName = "BatchElement";
JSONConstants.RequestMsgTypeName = "Message.OperationRequest";
JSONConstants.ResponseMsgTypeName = "Message.OperationResponse";
JSONConstants.DataMsgTypeName = "Message.Data";
JSONConstants.MDCMsgTypeName = "Message.MDC";
JSONConstants.OperationName = "Operation";
JSONConstants.PKCS7TypeName = "Sign.PKCS7";
JSONConstants.RawTypeName = "Sign.Raw";
JSONConstants.RequestNameName = "MainOperation";
JSONConstants.ReqHashAlgName = "HashAlgorithm";
JSONConstants.ReqHashName = "Source";
JSONConstants.ReqCertsName = "RequestedCerts";
JSONConstants.SigMsgTypeName = "Message.Sig";
JSONConstants.SigTypeName = "SigType";
JSONConstants.SigValueName = "SigValue";
JSONConstants.SigParamsName = "SigParams";
JSONConstants.SigParamName = "Param";
JSONConstants.SigHashAlgName = "HashAlg";
JSONConstants.SigSaltName = "Salt";
JSONConstants.SigKeyIDName = "KeyID";
JSONConstants.OriginalMessageName = "OriginalMessage";
JSONConstants.OperationResultName = "OperationResult";
JSONConstants.MsgKeysName = "Keys";
JSONConstants.MsgKeyName = "Key";
JSONConstants.ValueName = "value";
JSONConstants.OIDName = "oid";
JSONConstants.TagName = "tag";

//== GeneralConstOID
// Class with general OID constants
function GeneralConstOID() {}
GeneralConstOID.SIGN_DC_CERTIFICATE = "2.35.105.103.110.105.110.103.45.99.101.114.116.105.102.105.99.97.116.101.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
GeneralConstOID.OTHER_DC_CERTIFICATE = "2.19.101.114.116.105.102.105.99.97.116.101.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";

//== PKCS7ConstOID
// Class with PKCS7 OID constants
function PKCS7ConstOID() {}

PKCS7ConstOID.INSERT_SIGNING_TIME = "2.20.99.112.55.105.110.115.101.114.116.115.105.103.110.105.110.103.116.105.109.101.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
PKCS7ConstOID.INSERT_MESSAGE_DIGESTS = "2.20.99.112.55.105.110.115.101.114.116.109.100.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
PKCS7ConstOID.INSERT_CONTENT_TYPE = "2.20.99.112.55.105.110.115.101.114.116.99.116.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
PKCS7ConstOID.INSERT_SIGNINGCERT_ATTR = "2.20.99.112.55.105.110.115.101.114.116.115.105.103.110.105.110.103.99.101.114.116.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
PKCS7ConstOID.USE_GENERALIZED_TIME_FORMAT = "2.20.99.112.55.117.115.101.103.101.110.116.105.109.101.102.111.114.109.97.116.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
PKCS7ConstOID.FORCE_SIGNINGCERT_V2 = "2.20.99.112.55.102.111.114.99.101.115.105.103.110.105.110.103.99.101.114.116.118.50.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
PKCS7ConstOID.USE_UNDEF_SIZE = "2.20.99.112.55.117.115.101.117.110.100.101.102.115.105.122.101.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
PKCS7ConstOID.USE_PSS = "2.20.99.112.55.117.115.101.112.115.115.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
PKCS7ConstOID.INCLUDE_CERTIFICATES = "2.20.99.112.55.105.110.99.108.117.100.101.99.101.114.116.115.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
PKCS7ConstOID.INCLUDE_CRLS = "2.20.99.112.55.105.110.99.108.117.100.101.99.114.108.115.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
PKCS7ConstOID.INCLUDE_REVINFO_TO_ATTRS = "2.20.99.112.55.105.110.99.108.117.100.101.114.101.118.105.110.102.111.116.111.97.116.116.114.115.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
PKCS7ConstOID.INCLUDE_CHAIN = "2.20.99.112.55.105.110.99.108.117.100.101.99.104.97.105.110.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
PKCS7ConstOID.SIGNED_ATTRIBUTES = "2.20.99.112.55.115.105.103.110.101.100.97.116.116.114.115.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
PKCS7ConstOID.UNSIGNED_ATTRIBUTES = "2.20.99.112.55.117.110.115.105.103.110.101.100.97.116.116.114.115.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
PKCS7ConstOID.SIGNING_TIME = "2.20.99.112.55.115.105.103.110.105.110.103.116.105.109.101.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
PKCS7ConstOID.CONTENT_TYPE = "2.20.99.112.55.99.111.110.116.101.110.116.116.121.112.101.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
PKCS7ConstOID.TIMESTAMP = "2.20.99.112.55.116.105.109.101.115.116.97.109.112.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
PKCS7ConstOID.TIMESTAMP_SERVICE_ID = "2.20.99.112.55.116.115.115.105.100.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
PKCS7ConstOID.SGNDATA_VERSION = "2.20.99.112.55.115.103.110.100.97.116.97.118.101.114.115.105.111.110.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
PKCS7ConstOID.TIMESTAMP_HASH_ALGORITHM = "2.20.99.112.55.116.115.104.97.115.104.97.108.103.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
//PKCS7ConstOID.CUSTOMOP_PFX = "2.20.99.112.55.99.117.115.116.111.109.111.112.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109.45";
PKCS7ConstOID.IGNORE_TIMESTAMP_FAILURE = "2.20.99.112.55.105.103.110.111.114.101.116.115.102.97.105.108.117.114.101.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";

//== RawConstOID
// Class with Raw OID constants
function RawConstOID() {}

RawConstOID.USE_PSS = "2.20.99.120.53.48.57.117.115.101.112.115.115.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
RawConstOID.FIXED_PSS_PARAMS = "2.20.99.120.53.48.57.102.105.120.101.100.112.115.115.112.97.114.97.109.115.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
RawConstOID.PSS_SALT_SIZE = "2.20.99.120.53.48.57.112.115.115.115.97.108.116.115.105.122.101.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
RawConstOID.PSS_TRAILER_FIELD = "2.20.99.120.53.48.57.112.115.115.116.114.97.105.108.101.114.102.105.101.108.100.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";
RawConstOID.PSS_MGF_ALGORITHM = "2.20.99.120.53.48.57.112.115.115.109.103.102.97.108.103.111.114.105.116.104.109.64.115.101.99.117.114.101.98.108.97.99.107.98.111.120.46.99.111.109";

//== Converting
// Class with converting functions
function Converting() {}

Converting.stringToBool = function(val)
{
	return (val === "true" || val === "TRUE")
}

Converting.boolToString = function(val)
{
	return val ? "true" : "false";
}

Converting.hexStringToBytes = function(str)
{
	for (var bytes = [], c = 0; c < str.length; c += 2)
		bytes.push(parseInt(str.substr(c, 2), 16));

	return bytes;
}

Converting.bytesToHexString = function(byteArray) {
	return Array.from(byteArray, function(byte) {
		return ('0' + (byte & 0xFF).toString(16)).slice(-2);
	}).join('').toUpperCase();
}

Converting.bytesToOIDString = function(oid)
{
	var res = "";
	var i = 0;
	
	while (i < oid.length)
	{
		if (i === 0)
		{
			let b = oid[0] % 40;
            let a = (oid[0] - b) / 40;
			res += a + "." + b;
		}
		else
		{
			if (oid[i] < 128)
				res += "." + oid[i];
			else
			{
                let a = 0;
				while (oid[i] >= 128)
				{
					a = (a + (oid[i] - 128)) * 128;
					i++;
				}
				res += "." + (a + oid[i]);
			}
		}
		
		i++;
	}

	return res;
}

Converting.oidStringToBytes = function(oid)
{
	oid = oid.trim();
	oid = oid.replace(/^\.+/, "");
	oid = oid.replace(/\.+$/, "");
	oid = oid.trim();
	var split = oid.split(".");
	var retVal = new Array();

	for (var a = 0, b = 0, i = 0; i < split.length; i++)
	{
		if (i === 0)
			a = parseInt(split[0]);
		else if (i === 1)
			retVal.push(40 * a + parseInt(split[1]));
		else
		{
			b = parseInt(split[i]);

			if (b < 128)
				retVal.push(b);
			else
			{
				var tmp = [b%128];
				b = Math.trunc(b/128);
				while (b >= 128)
				{
					tmp.unshift(128 + b%128);
					b = Math.trunc(b/128);
				}
				tmp.unshift(128 + b);
				retVal = retVal.concat(tmp);
			}
		}
	}

	return retVal;
}

Converting.hexStringToOIDString = function(str)
{
	return this.bytesToOIDString(this.hexStringToBytes(str));
}

Converting.oidStringToHexString = function(oid)
{
	return this.bytesToHexString(this.oidStringToBytes(oid));
}

Converting.bytesToString = function(array)
{
	var result = "";
	for (var i = 0; i < array.length; i++)
		result += String.fromCharCode(array[i]);

	return result;
}

Converting.stringToBytes = function(str)
{
	var result = [];
	for (var i = 0; i < str.length; i++)
		result.push(str.charCodeAt(i));

	return result;
}

Converting.hexStringToString = function(val)
{
	return this.bytesToString(this.hexStringToBytes(val));
}

Converting.stringToHexString = function(val)
{
	return this.bytesToHexString(this.stringToBytes(val));
}

Converting.hexStringToBool = function(str)
{
	return this.stringToBool(this.hexStringToString(str));
}

Converting.boolToHexString = function(val)
{
	return this.stringToHexString(this.boolToString(val));
}

Converting.generalizedTimeStringToDate = function(str)
{
	var res = new Date();
	res.setFullYear(parseInt(str.substring(0, 4)), parseInt(str.substring(4, 6)) - 1, parseInt(str.substring(6, 8)));
	res.setHours(parseInt(str.substring(8, 10)), parseInt(str.substring(10, 12)), parseInt(str.substring(12, 14)));

	return res;
}

Converting.dateToGeneralizedTimeString = function(val)
{
	function numToString(num, size) {
		var s = "0000" + num;
		return s.substr(s.length-size);
	}

	return numToString(val.getFullYear(), 4) + numToString(val.getMonth() + 1, 2) + numToString(val.getDate(), 2) + 
		numToString(val.getHours(), 2) + numToString(val.getMinutes(), 2) + numToString(val.getSeconds(), 2) + "Z";
}

Converting.hexStringToData = function(str)
{
	return this.generalizedTimeStringToDate(this.hexStringToString(str));
}

Converting.dataToHexString = function(val)
{
	return this.stringToHexString(this.dateToGeneralizedTimeString(val));
}

Converting.bytesToInt64LE = function(array)
{
	var t1 = array[0] + (array[1] << 8) + (array[2] << 16) + (array[3] << 24);
	var t2 = array[4] + (array[5] << 8) + (array[6] << 16) + (array[7] << 24);

	return t1 + (t2 << 32);
}

Converting.int64LEToBytes = function(val)
{
	var bytes = [];
	var i = 0;
	do
	{
		bytes[i++] = val & (255);
		val = val >> 8;
	} while (i < 8)
	
	return bytes;
}

Converting.hexStringToInt = function(str)
{
	return this.bytesToInt64LE(this.hexStringToBytes(str));
}

Converting.intToHexString = function(val)
{
	return this.bytesToHexString(this.int64LEToBytes(val));
}

// Generate hex string
function generateId(length)
{
	var result = [];
	var characters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
	var charactersLength = characters.length;
	for ( var i = 0; i < length; i++ )
		result.push(characters.charAt(Math.floor(Math.random() * charactersLength)));

	return result.join("");
}

//== DcauthAsyncRequest
// Can receive one parameter with async state JSON string
function DcauthAsyncRequest()
{
	this.getType = function() { return this.state[JSONConstants.RootName][JSONConstants.TypeName]; };
	this.getGenerator = function() { return this.state[JSONConstants.RootName][JSONConstants.GeneratorName]; };
	this.getRequestType = function()
	{
		if (this.findRequestBatch()[JSONConstants.OperationName] === JSONConstants.PKCS7TypeName)
			return "pkcs7";
		else
			return "raw";
	};
	
	// request parameters
	this._params = {};
	this._params.owner = this;
	this.getParams = function() { return this._params; };
	
	this.getHashAlgorithm = function() { return Converting.hexStringToOIDString(this.findRequestBatch()[JSONConstants.ReqHashAlgName]); };
	this.getHash = function() { return this.findRequestBatch()[JSONConstants.ReqHashName]; };
	this.getIncludeCerts = function() { return this.findRequestBatch()[JSONConstants.ReqCertsName]; };
	this.getSigned = function() { return this.findSignatureBatch() !== null; };

	// signature parameters
	this._signatureInfo = {};
	this._signatureInfo.owner = this;
	this._signatureInfo.getType = function()
	{
		if (this.owner.findSignatureBatch() !== null)
			return this.owner.findSignatureBatch()[JSONConstants.SigTypeName];
		else
			return "";
	};
	this._signatureInfo.getValue = function()
	{
		if (this.owner.findSignatureBatch() !== null)
			return this.owner.findSignatureBatch()[JSONConstants.SigValueName];
		else
			return "";
	};
	this._signatureInfo.getHashAlgorithm = function()
	{
		if (this.owner.findSignatureBatch() !== null)
		{
			const value = this.owner.findSigParamValue(JSONConstants.SigHashAlgName);
			if (value !== undefined)
				return value;
			else
				return "";
		}
		else
			return "";
	};
	this._signatureInfo.getSalt = function()
	{
		if (this.owner.findSignatureBatch() !== null)
		{
			const value = this.owner.findSigParamValue(JSONConstants.SigSaltName);
			if (value !== undefined)
				return value;
			else
				return "";
		}
		else
			return "";
	};
	this._signatureInfo.getKeyID = function()
	{
		if (this.owner.findSignatureBatch() !== null)
		{
			const value = this.owner.findSigParamValue(JSONConstants.SigKeyIDName);
			if (value !== undefined)
				return value;
			else
				return "";
		}
		else
			return "";
	};
	this.getSignatureInfo = function() { return this._signatureInfo; };

	// Loading data if receive parameter
	if (arguments.length > 0) 
	{
		if (typeof arguments[0] == "string")
			this.loadfromstring(arguments[0]);
		else
			this.load(arguments[0]);
	}
	else
	{
		this.createEmptyState();
	}
}

// Update set of request parameters
DcauthAsyncRequest.prototype.updateParams = function()
{
	delete this._params.getContentType;
	delete this._params.getSgnDataVersion;
	delete this._params.getSigningTime;
	delete this._params.getTimestamp;
	delete this._params.getTimestampServiceID;
	delete this._params.getTimestampHashAlgorithm;
	delete this._params.getIncludeCertsToSgnData;
	delete this._params.getIncludeCRLsToSgnData;
	delete this._params.getIncludeRevInfoToAttributes;
	delete this._params.getIncludeCertChain;
	delete this._params.getInsertMessageDigests;
	delete this._params.getInsertSigningTime;
	delete this._params.getInsertContentType;
	delete this._params.getInsertSigningCertificateAttr;
	delete this._params.getIgnoreTimestampFailure;
	delete this._params.getUseGeneralizedTimeFormat;
	delete this._params.getForceSigningCertificateV2Usage;
	delete this._params.getUseUndefSize;
	delete this._params.getUsePSS;
	delete this._params.getCustomSignedAttributes;
	delete this._params.getCustomUnsignedAttributes;
	delete this._params.getFixedPSSParams;
	delete this._params.getSaltSize;
	delete this._params.getTrailerField;
	delete this._params.getMgfAlgorithm;

	if (this.getRequestType() === "pkcs7")
	{
		this._params.getContentType = function()
		{
			const value = this.owner.findParamValue(PKCS7ConstOID.CONTENT_TYPE);
			if (value !== undefined)
				return Converting.hexStringToOIDString(value);
			else
				return "";
		};
		this._params.getSgnDataVersion = function()
		{
			const value = this.owner.findParamValue(PKCS7ConstOID.SGNDATA_VERSION);
			if (value !== undefined)
				return Converting.hexStringToInt(value);
			else
				return 0;
		};
		this._params.getSigningTime = function()
		{
			const value = this.owner.findParamValue(PKCS7ConstOID.SIGNING_TIME);
			if (value !== undefined)
				return Converting.hexStringToData(value);
			else
				return null;
		};
		this._params.getTimestamp = function()
		{
			const value = this.owner.findParamValue(PKCS7ConstOID.TIMESTAMP);
			if (value !== undefined)
				return Converting.hexStringToBool(value);
			else
				return false;
		};
		this._params.getTimestampServiceID = function()
		{
			const value = this.owner.findParamValue(PKCS7ConstOID.TIMESTAMP_SERVICE_ID);
			if (value !== undefined)
				return Converting.hexStringToBytes(value);
			else
				return [];
		};
		this._params.getTimestampHashAlgorithm = function()
		{
			const value = this.owner.findParamValue(PKCS7ConstOID.TIMESTAMP_HASH_ALGORITHM);
			if (value !== undefined)
				return Converting.hexStringToOIDString(value);
			else
				return "";
		};
		this._params.getIncludeCertsToSgnData = function()
		{
			const value = this.owner.findParamValue(PKCS7ConstOID.INCLUDE_CERTIFICATES);
			if (value !== undefined)
				return Converting.hexStringToBool(value);
			else
				return true;
		};
		this._params.getIncludeCRLsToSgnData = function()
		{
			const value = this.owner.findParamValue(PKCS7ConstOID.INCLUDE_CRLS);
			if (value !== undefined)
				return Converting.hexStringToBool(value);
			else
				return false;
		};
		this._params.getIncludeRevInfoToAttributes = function()
		{
			const value = this.owner.findParamValue(PKCS7ConstOID.INCLUDE_REVINFO_TO_ATTRS);
			if (value !== undefined)
				return Converting.hexStringToBool(value);
			else
				return false;
		};
		this._params.getIncludeCertChain = function()
		{
			const value = this.owner.findParamValue(PKCS7ConstOID.INCLUDE_CHAIN);
			if (value !== undefined)
				return Converting.hexStringToBool(value);
			else
				return true;
		};
		this._params.getInsertMessageDigests = function()
		{
			const value = this.owner.findParamValue(PKCS7ConstOID.INSERT_MESSAGE_DIGESTS);
			if (value !== undefined)
				return Converting.hexStringToBool(value);
			else
				return true;
		};
		this._params.getInsertSigningTime = function()
		{
			const value = this.owner.findParamValue(PKCS7ConstOID.INSERT_SIGNING_TIME);
			if (value !== undefined)
				return Converting.hexStringToBool(value);
			else
				return true;
		};
		this._params.getInsertContentType = function()
		{
			const value = this.owner.findParamValue(PKCS7ConstOID.INSERT_CONTENT_TYPE);
			if (value !== undefined)
				return Converting.hexStringToBool(value);
			else
				return true;
		};
		this._params.getInsertSigningCertificateAttr = function()
		{
			const value = this.owner.findParamValue(PKCS7ConstOID.INSERT_SIGNINGCERT_ATTR);
			if (value !== undefined)
				return Converting.hexStringToBool(value);
			else
				return true;
		};
		this._params.getIgnoreTimestampFailure = function()
		{
			const value = this.owner.findParamValue(PKCS7ConstOID.IGNORE_TIMESTAMP_FAILURE);
			if (value !== undefined)
				return Converting.hexStringToBool(value);
			else
				return true;
		};
		this._params.getUseGeneralizedTimeFormat = function()
		{
			const value = this.owner.findParamValue(PKCS7ConstOID.USE_GENERALIZED_TIME_FORMAT);
			if (value !== undefined)
				return Converting.hexStringToBool(value);
			else
				return true;
		};
		this._params.getForceSigningCertificateV2Usage = function()
		{
			const value = this.owner.findParamValue(PKCS7ConstOID.FORCE_SIGNINGCERT_V2);
			if (value !== undefined)
				return Converting.hexStringToBool(value);
			else
				return true;
		};
		this._params.getUseUndefSize = function()
		{
			const value = this.owner.findParamValue(PKCS7ConstOID.USE_UNDEF_SIZE);
			if (value !== undefined)
				return Converting.hexStringToBool(value);
			else
				return true;
		};
		this._params.getUsePSS = function()
		{
			const value = this.owner.findParamValue(PKCS7ConstOID.USE_PSS);
			if (value !== undefined)
				return Converting.hexStringToBool(value);
			else
				return false;
		};
		this._params.getCustomSignedAttributes = function()
		{
			const value = this.owner.findParamValue(PKCS7ConstOID.SIGNED_ATTRIBUTES);
			if (value !== undefined)
				return Converting.hexStringToBytes(value);
			else
				return [];
		};
		this._params.getCustomUnsignedAttributes = function()
		{
			const value = this.owner.findParamValue(PKCS7ConstOID.UNSIGNED_ATTRIBUTES);
			if (value !== undefined)
				return Converting.hexStringToBytes(value);
			else
				return [];
		};
	}
	else
	{
		this._params.getUsePSS = function()
		{
			const value = this.owner.findParamValue(RawConstOID.USE_PSS);
			if (value !== undefined)
				return Converting.hexStringToBool(value);
			else
				return false;
		};
		this._params.getFixedPSSParams = function()
		{
			const value = this.owner.findParamValue(RawConstOID.FIXED_PSS_PARAMS);
			if (value !== undefined)
				return Converting.hexStringToBool(value);
			else
				return false;
		};
		this._params.getSaltSize = function()
		{
			const value = this.owner.findParamValue(RawConstOID.PSS_SALT_SIZE);
			if (value !== undefined)
				return Converting.hexStringToInt(value);
			else
				return 0;
		};
		this._params.getTrailerField = function()
		{
			const value = this.owner.findParamValue(RawConstOID.PSS_TRAILER_FIELD);
			if (value !== undefined)
				return Converting.hexStringToInt(value);
			else
				return 0;
		};
		this._params.getMgfAlgorithm = function()
		{
			const value = this.owner.findParamValue(RawConstOID.PSS_MGF_ALGORITHM);
			if (value !== undefined)
				return Converting.hexStringToInt(value);
			else
				return 0;
		};
	}
}

// Get request batch from state
DcauthAsyncRequest.prototype.findRequestBatch = function(state)
{
	var result = null;

	if (arguments.length === 0)
		state = this.state;

	var batches = state[JSONConstants.RootName][JSONConstants.MsgName][JSONConstants.BatchName];
	for (var i = 0; i < batches.length; i++)
	{
		if (batches[i][JSONConstants.MsgTypeName] === JSONConstants.RequestMsgTypeName)
		{
			result = batches[i];
			break;
		}
	}

	return result;
}

// Get request batch from state
DcauthAsyncRequest.prototype.findSignatureBatch = function(state)
{
	var result = null;

	if (arguments.length === 0)
		state = this.state;

	var batches = state[JSONConstants.RootName][JSONConstants.MsgName][JSONConstants.BatchName];
	for (var i = 0; i < batches.length; i++)
	{
		if (batches[i][JSONConstants.MsgTypeName] === JSONConstants.SigMsgTypeName)
		{
			result = batches[i];
			break;
		}
	}

	return result;
}

// Get request parameter value by name
DcauthAsyncRequest.prototype.findParamValue = function(name)
{
	var result = undefined;

	if (!(JSONConstants.MsgParamName in this.findRequestBatch()[JSONConstants.MsgParamsName]))
		return result;

	var reqParams = this.findRequestBatch()[JSONConstants.MsgParamsName][JSONConstants.MsgParamName];

	for (var i = 0; i < reqParams.length; i++)
	{
		if (Converting.hexStringToOIDString(reqParams[i][JSONConstants.OIDName]) === name)
		{
			result = reqParams[i][JSONConstants.ValueName];
			break;
		}
	}

	return result;
}

// Get signature parameter value by name
DcauthAsyncRequest.prototype.findSigParamValue = function(name)
{
	var result = undefined;

	var sigParams = this.findSignatureBatch()[JSONConstants.SigParamsName][JSONConstants.SigParamName];

	for (var i = 0; i < sigParams.length; i++)
	{
		const value = sigParams[i][JSONConstants.ValueName].split("=");
		if (value[0] === name)
		{
			result = value[1];
			break;
		}
	}

	return result;
}

// generate empty state
DcauthAsyncRequest.prototype.createEmptyState = function()
{
	this.state = {};

	var rootObj = {};
	rootObj[JSONConstants.TypeName] = "";
	rootObj[JSONConstants.SubtypesName] = {};
	rootObj[JSONConstants.SubtypesName][JSONConstants.SubtypeName] = JSONConstants.RawSubtypeName;
	rootObj[JSONConstants.GeneratorName] = "";

	var rootMsg = {}
	rootMsg[JSONConstants.MsgTypeName] = JSONConstants.RootMsgTypeName;
	rootMsg[JSONConstants.MsgIdName] = generateId(32);
	rootMsg[JSONConstants.MsgNameName] = "";
	rootMsg[JSONConstants.MsgParamsName] = "";
	rootMsg[JSONConstants.BatchName] = [];

	// save request
	request = {};
	request[JSONConstants.MsgTypeName] = JSONConstants.RequestMsgTypeName;
	request[JSONConstants.MsgIdName] = generateId(32);
	request[JSONConstants.MsgNameName] = JSONConstants.RequestNameName;

	// save request parameters
	var params = [];
	// USE_PSS
	params[0] = {};
	params[0][JSONConstants.ValueName] = Converting.boolToHexString(false);
	params[0][JSONConstants.OIDName] = Converting.oidStringToHexString(RawConstOID.USE_PSS);
	params[0][JSONConstants.TagName] = 4;
	// FIXED_PSS_PARAMS
	params[1] = {};
	params[1][JSONConstants.ValueName] = Converting.boolToHexString(false);
	params[1][JSONConstants.OIDName] = Converting.oidStringToHexString(RawConstOID.FIXED_PSS_PARAMS);
	params[1][JSONConstants.TagName] = 4;
	// PSS_SALT_SIZE
	params[2] = {};
	params[2][JSONConstants.ValueName] = Converting.intToHexString(0);
	params[2][JSONConstants.OIDName] = Converting.oidStringToHexString(RawConstOID.PSS_SALT_SIZE);
	params[2][JSONConstants.TagName] = 4;
	// PSS_TRAILER_FIELD
	params[3] = {};
	params[3][JSONConstants.ValueName] = Converting.intToHexString(0);
	params[3][JSONConstants.OIDName] = Converting.oidStringToHexString(RawConstOID.PSS_TRAILER_FIELD);
	params[3][JSONConstants.TagName] = 4;
	// PSS_MGF_ALGORITHM
	params[4] = {};
	params[4][JSONConstants.ValueName] = Converting.intToHexString(0);
	params[4][JSONConstants.OIDName] = Converting.oidStringToHexString(RawConstOID.PSS_MGF_ALGORITHM);
	params[4][JSONConstants.TagName] = 4;

	request[JSONConstants.MsgParamsName] = {};
	request[JSONConstants.MsgParamsName][JSONConstants.MsgParamName] = params;

	request[JSONConstants.OperationName] = JSONConstants.RawTypeName;
	request[JSONConstants.ReqHashName] = "";
	request[JSONConstants.ReqHashAlgName] = "";
	request[JSONConstants.ReqCertsName] = "";

	rootMsg[JSONConstants.BatchName][0] = request;

	rootObj[JSONConstants.MsgName] = rootMsg;

	this.state[JSONConstants.RootName] = rootObj;
	
	// update parameters property
	this.updateParams();
}


// Load data from JSON string
DcauthAsyncRequest.prototype.loadfromstring = function(asyncStateJSON)
{
	this.load(JSON.parse(asyncStateJSON))
}

// Load data from JSON object
DcauthAsyncRequest.prototype.load = function(asyncStateJSON)
{
	// check correct state's structure
	if (JSONConstants.RootName in asyncStateJSON)
	{
		if (!(JSONConstants.TypeName in asyncStateJSON[JSONConstants.RootName]))
			throw new Error(ErrorMessages.InvalidRequest);

		if (!(JSONConstants.GeneratorName in asyncStateJSON[JSONConstants.RootName]))
			throw new Error(ErrorMessages.InvalidRequest);

		if (JSONConstants.MsgName in asyncStateJSON[JSONConstants.RootName] && JSONConstants.BatchName in asyncStateJSON[JSONConstants.RootName][JSONConstants.MsgName])
		{
			var requestBatch = this.findRequestBatch(asyncStateJSON);
			var sigBatch = this.findSignatureBatch(asyncStateJSON);

			if (requestBatch === null)
				throw new Error(ErrorMessages.InvalidRequest);

			if (!(JSONConstants.MsgParamsName in requestBatch))
				throw new Error(ErrorMessages.InvalidRequest);

			if (sigBatch !== null)
			{
				if (!(JSONConstants.SigTypeName in sigBatch))
					throw new Error(ErrorMessages.InvalidRequest);
				if (!(JSONConstants.SigValueName in sigBatch))
					throw new Error(ErrorMessages.InvalidRequest);

				if (!(JSONConstants.SigParamsName in sigBatch && JSONConstants.SigParamName in sigBatch[JSONConstants.SigParamsName]))
					throw new Error(ErrorMessages.InvalidRequest);
			}

			// init/update state property
			this.state = asyncStateJSON;

			// update parameters property
			this.updateParams();
		}
		else
			throw new Error(ErrorMessages.InvalidRequest);
	}
	else
		throw new Error(ErrorMessages.InvalidRequest);
}

// Save data to JSON object
DcauthAsyncRequest.prototype.save = function()
{
	return this.state;
}

// Save data to JSON string
DcauthAsyncResponse.prototype.savetostring = function()
{
	return JSON.stringify(this.state);
}

function DcauthAsyncResponse()
{
	this.getType = function() { return this.state[JSONConstants.RootName][JSONConstants.TypeName]; };
	this.setType = function(value)
	{
		if (!(typeof value === "string"))
			throw new Error(ErrorMessages.InvalidValueType);

		this.state[JSONConstants.RootName][JSONConstants.TypeName] = value;
	};

	this.getGenerator = function() { return this.state[JSONConstants.RootName][JSONConstants.GeneratorName]; };
	this.setGenerator = function(value)
	{
		if (!(typeof value === "string"))
			throw new Error(ErrorMessages.InvalidValueType);

		this.state[JSONConstants.RootName][JSONConstants.GeneratorName] = value;
	};

	this.getSignature = function() { return this.findResponseBatch()[JSONConstants.OperationResultName]; };
	this.setSignature = function(value)
	{
		if (!(typeof value === "string"))
			throw new Error(ErrorMessages.InvalidValueType);

		this.findResponseBatch()[JSONConstants.OperationResultName] = value;
	};

	this.getSigningCertificate = function()
	{
		var keys = this.findResponseBatch()[JSONConstants.MsgKeysName][JSONConstants.MsgKeyName];
		if (keys instanceof Array)
		{
			for (var i = 0; i < keys.length; i++)
			{
				if (Converting.hexStringToOIDString(keys[i][JSONConstants.OIDName]) === GeneralConstOID.SIGN_DC_CERTIFICATE)
					return keys[i][JSONConstants.ValueName];
			}

			// if not found signing certificate
			return "";
		}
		else if (keys instanceof Object)
		{
			if (Converting.hexStringToOIDString(keys[JSONConstants.OIDName]) === GeneralConstOID.SIGN_DC_CERTIFICATE)
				return keys[JSONConstants.ValueName]
			else
				return "";
		}
		else
		{
			return "";
		}
	};
	this.setSigningCertificate = function(value)
	{
		if (!(typeof value === "string"))
			throw new Error(ErrorMessages.InvalidValueType);

		var keys = this.findResponseBatch()[JSONConstants.MsgKeysName][JSONConstants.MsgKeyName];
		if (keys instanceof Array)
		{
			var signCertIdx = -1;
			for (var i = 0; i < keys.length; i++)
			{
				if (Converting.hexStringToOIDString(keys[i][JSONConstants.OIDName]) === GeneralConstOID.SIGN_DC_CERTIFICATE)
				{
					signCertIdx = i;
					break;
				}
			}

			if (signCertIdx !== -1)
				keys[signCertIdx][JSONConstants.ValueName] = value;
			else
			{
				var newSignCert = {};
				newSignCert[JSONConstants.OIDName] = Converting.oidStringToHexString(GeneralConstOID.SIGN_DC_CERTIFICATE);
				newSignCert[JSONConstants.ValueName] = value;
				newSignCert[JSONConstants.TagName] = 4;
				keys.push(newSignCert);
			}
		}
		else if (keys instanceof Object)
		{
			if (Converting.hexStringToOIDString(keys[JSONConstants.OIDName]) === GeneralConstOID.SIGN_DC_CERTIFICATE)
				keys[JSONConstants.ValueName] = value;
			else
			{
				//copy object to array
				var newSignCert = {};
				newSignCert[JSONConstants.OIDName] = keys[JSONConstants.OIDName];
				newSignCert[JSONConstants.ValueName] = keys[JSONConstants.ValueName];
				newSignCert[JSONConstants.TagName] = keys[JSONConstants.TagName];
				//remove old object properties
				delete keys[JSONConstants.OIDName];
				delete keys[JSONConstants.ValueName];
				delete keys[JSONConstants.TagName];
				// init array and copy key to it
				keys = [];
				keys.push(newSignCert);
				//add new certificate
				newSignCert = {};
				newSignCert[JSONConstants.OIDName] = Converting.oidStringToHexString(GeneralConstOID.SIGN_DC_CERTIFICATE);
				newSignCert[JSONConstants.ValueName] = value;
				newSignCert[JSONConstants.TagName] = 4;
				keys.push(newSignCert);
			}
		}
		else
		{
			keys = {};
			keys[JSONConstants.OIDName] = Converting.oidStringToHexString(GeneralConstOID.SIGN_DC_CERTIFICATE);
			keys[JSONConstants.ValueName] = value;
			keys[JSONConstants.TagName] = 4;
		}

		//update keys
		this.findResponseBatch()[JSONConstants.MsgKeysName][JSONConstants.MsgKeyName] = keys;
	};

	this.getSigningChain = function()
	{
		var result = [];

		var keys = this.findResponseBatch()[JSONConstants.MsgKeysName][JSONConstants.MsgKeyName];
		if (keys instanceof Array)
		{
			for (var i = 0; i < keys.length; i++)
			{
				if (Converting.hexStringToOIDString(keys[i][JSONConstants.OIDName]) === GeneralConstOID.OTHER_DC_CERTIFICATE)
					result.push(keys[i][JSONConstants.ValueName]);
			}
		}
		else if (keys instanceof Object)
		{
			if (Converting.hexStringToOIDString(keys[JSONConstants.OIDName]) === GeneralConstOID.OTHER_DC_CERTIFICATE)
				result.push(keys[JSONConstants.ValueName]);
		}

		return result;
	};
	this.setSigningChain = function(value)
	{
		if (typeof value === "string")
			value = [value];
		else if (arguments.length === 0)
			value = [];
		else if (!(value instanceof Array))
			throw new Error(ErrorMessages.InvalidValueType);

		var keys = this.findResponseBatch()[JSONConstants.MsgKeysName][JSONConstants.MsgKeyName];
		if (keys instanceof Array)
		{
			var i = 0
			while (i < keys.length)
			{
				if (Converting.hexStringToOIDString(keys[i][JSONConstants.OIDName]) === GeneralConstOID.OTHER_DC_CERTIFICATE)
					keys.splice(i, 1);
				else
					i++;
			}
		}
		else if (keys instanceof Object)
		{
			if (Converting.hexStringToOIDString(keys[JSONConstants.OIDName]) === GeneralConstOID.OTHER_DC_CERTIFICATE)
			{
				if (value.length === 1)
				{
					keys[JSONConstants.ValueName] = value[0];

					//update keys
					this.findResponseBatch()[JSONConstants.MsgKeysName][JSONConstants.MsgKeyName] = keys;

					return;
				}
				else
				{
					delete keys[JSONConstants.OIDName];
					delete keys[JSONConstants.ValueName];
					delete keys[JSONConstants.TagName];

					if (value.length > 1)
						keys = [];
					else
					{
						//remove key element
						this.findResponseBatch()[JSONConstants.MsgKeysName] = {};

						return;
					}
				}
			}
			else
			{
				if (value.length > 0)
				{
					//copy sign cert to new object
					var newSignCert = {};
					newSignCert[JSONConstants.OIDName] = keys[JSONConstants.OIDName];
					newSignCert[JSONConstants.ValueName] = keys[JSONConstants.ValueName];
					newSignCert[JSONConstants.TagName] = keys[JSONConstants.TagName];
					//remove old object properties
					delete keys[JSONConstants.OIDName];
					delete keys[JSONConstants.ValueName];
					delete keys[JSONConstants.TagName];
					// init array and copy sign cert to it
					keys = [];
					keys.push(newSignCert);
				}
			}
		}
		else
		{
			if (value.length === 1)
			{
				keys = {};
				keys[JSONConstants.OIDName] = Converting.oidStringToHexString(GeneralConstOID.OTHER_DC_CERTIFICATE);
				keys[JSONConstants.ValueName] = value[0];
				keys[JSONConstants.TagName] = 4;

				//update keys
				this.findResponseBatch()[JSONConstants.MsgKeysName][JSONConstants.MsgKeyName] = keys;

				return;
			}
			else if (value.length > 0)
				keys = [];
			else
			{
				//remove key element
				this.findResponseBatch()[JSONConstants.MsgKeysName] = {};

				return;
			}
		}

		// add new signing chain to array
		for (var i = 0; i < value.length; i++)
		{
			var newSignCert = {};
			newSignCert[JSONConstants.OIDName] = Converting.oidStringToHexString(GeneralConstOID.OTHER_DC_CERTIFICATE);
			newSignCert[JSONConstants.ValueName] = value[i];
			newSignCert[JSONConstants.TagName] = 4;
			keys.push(newSignCert);
		}

		//update keys
		this.findResponseBatch()[JSONConstants.MsgKeysName][JSONConstants.MsgKeyName] = keys;
	};

	// Loading data if receive parameter with request
	if (arguments.length > 0 && arguments[0] instanceof DcauthAsyncRequest) 
	{
		this.createFromRequest(arguments[0]);
	}
	else // if no parameter then generate empty state
	{
		this.createEmptyState();
	}
}

// Get response batch from state
DcauthAsyncResponse.prototype.findResponseBatch = function(state)
{
	var result = null;

	if (arguments.length === 0)
		state = this.state;

	var batches = state[JSONConstants.RootName][JSONConstants.MsgName][JSONConstants.BatchName];
	for (var i = 0; i < batches.length; i++)
	{
		if (batches[i][JSONConstants.MsgTypeName] === JSONConstants.ResponseMsgTypeName)
		{
			result = batches[i];
			break;
		}
	}

	return result;
}

// generate empty state
DcauthAsyncResponse.prototype.createEmptyState = function()
{
	this.state = {};

	var rootObj = {};
	rootObj[JSONConstants.TypeName] = "";
	rootObj[JSONConstants.SubtypesName] = {};
	rootObj[JSONConstants.SubtypesName][JSONConstants.SubtypeName] = JSONConstants.RawSubtypeName;
	rootObj[JSONConstants.GeneratorName] = "";

	var rootMsg = {}
	rootMsg[JSONConstants.MsgTypeName] = JSONConstants.RootMsgTypeName;
	rootMsg[JSONConstants.MsgIdName] = generateId(32);
	rootMsg[JSONConstants.MsgNameName] = "";
	rootMsg[JSONConstants.MsgParamsName] = "";
	rootMsg[JSONConstants.BatchName] = [];

	// response batch
	response = {};
	response[JSONConstants.MsgTypeName] = JSONConstants.ResponseMsgTypeName;
	response[JSONConstants.MsgIdName] = generateId(32);
	response[JSONConstants.MsgNameName] = "";
	response[JSONConstants.MsgParamsName] = {};
	response[JSONConstants.OriginalMessageName] = {};
	response[JSONConstants.OperationName] = JSONConstants.RawTypeName;
	response[JSONConstants.OperationResultName] = "";
	response[JSONConstants.MsgKeysName] = {};

	rootMsg[JSONConstants.BatchName][0] = response;

	rootObj[JSONConstants.MsgName] = rootMsg;

	this.state[JSONConstants.RootName] = rootObj;
}

// generate state from request
DcauthAsyncResponse.prototype.createFromRequest = function(request)
{
	this.createEmptyState()

	// set subtype
	this.state[JSONConstants.RootName][JSONConstants.SubtypesName] = JSON.parse(JSON.stringify(request.state[JSONConstants.RootName][JSONConstants.SubtypesName]));

	//get data from request
	this.setType(request.getType());
	this.setGenerator(request.getGenerator());

	//copy original request
	var response = this.findResponseBatch();
	response[JSONConstants.OriginalMessageName] = JSON.parse(JSON.stringify(request.findRequestBatch()));

	//mirrored MDC and Data messages
	var reqbatches = request.state[JSONConstants.RootName][JSONConstants.MsgName][JSONConstants.BatchName];
	var batches = this.state[JSONConstants.RootName][JSONConstants.MsgName][JSONConstants.BatchName];
	var num = batches.length - 1;
	for (var i = 0; i < reqbatches.length; i++)
	{
		if (reqbatches[i][JSONConstants.MsgTypeName] === JSONConstants.DataMsgTypeName)
		{
			num++;
			batches[num] = JSON.parse(JSON.stringify(reqbatches[i]));
		}

		if (reqbatches[i][JSONConstants.MsgTypeName] === JSONConstants.MDCMsgTypeName)
		{
			num++;
			batches[num] = JSON.parse(JSON.stringify(reqbatches[i]));
		}
	}

	response[JSONConstants.OperationName] = request.findRequestBatch()[JSONConstants.OperationName];
}

// Load data from JSON string
DcauthAsyncResponse.prototype.loadfromstring = function(asyncStateJSON)
{
	this.load(JSON.parse(asyncStateJSON))
}

// Load data from JSON object
DcauthAsyncResponse.prototype.load = function(asyncStateJSON)
{
	// check correct state's structure
	if (JSONConstants.RootName in asyncStateJSON)
	{
		if (!(JSONConstants.TypeName in asyncStateJSON[JSONConstants.RootName]))
			throw new Error(ErrorMessages.InvalidRequest);

		if (!(JSONConstants.GeneratorName in asyncStateJSON[JSONConstants.RootName]))
			throw new Error(ErrorMessages.InvalidRequest);

		if (JSONConstants.MsgName in asyncStateJSON[JSONConstants.RootName] && JSONConstants.BatchName in asyncStateJSON[JSONConstants.RootName][JSONConstants.MsgName])
		{
			var responseBatch = this.findResponseBatch(asyncStateJSON);

			if (responseBatch === null)
				throw new Error(ErrorMessages.InvalidRequest);

			if (!(JSONConstants.OriginalMessageName in responseBatch && JSONConstants.MsgKeysName in responseBatch))
				throw new Error(ErrorMessages.InvalidRequest);

			// init/update state property
			this.state = asyncStateJSON;
		}
		else
			throw new Error(ErrorMessages.InvalidRequest);
	}
	else
		throw new Error(ErrorMessages.InvalidRequest);
}

// Save data to JSON object
DcauthAsyncResponse.prototype.save = function()
{
	return this.state;
}

// Save data to JSON string
DcauthAsyncResponse.prototype.savetostring = function()
{
	return JSON.stringify(this.state);
}