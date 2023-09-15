import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.InputStreamReader;

import secureblackbox.*;

public class demosettings
{
	// Server configuration
	private String m_CertificateFile = "";
	private String m_CertificatePassword = "";
	private boolean m_SettingsFound = false;
	private String m_SettingsFile = "";
    private String m_PassiveModeHost = "";
	private int m_ListeningPort = 21;
    private String m_ListeningAddress = "";
	private boolean m_AllowAnonymous = true;
	private boolean m_ImplicitSSL = false;
	private boolean m_RequireTLS = false;
	private boolean m_RequireTLSForData = false;
	private Certificate m_ServerCertificate;
	private int m_PortRangeFrom = 0;
	private int m_PortRangeTo = 0;
    static private demosettings m_Settings = new demosettings();

	public demosettings()
	{
	}


    static public demosettings getSettings()
    {
        return m_Settings;
    }

    public String getCertificateFile()
	{
		return m_CertificateFile;
	}
    
    public void setCertificateFile(String value){
		m_CertificateFile = value;    	
    }

    public String getCertificatePassword()
	{
		return m_CertificatePassword;
	}

    public void setCertificatePassword(String value){
    	m_CertificatePassword = value;
    }

	public boolean getSettingsFound()
	{
		 return m_SettingsFound;
	}

	public void setSettingsFound(boolean value) {
		m_SettingsFound = value;
	}
	
	public int getListeningPort()
	{
		return m_ListeningPort;
	}

	public void setListeningPort(int value) {
		m_ListeningPort = value;
	}
	
    public String getListeningAddress()
    {
    	return m_ListeningAddress;
    }

    public void setListeningAddress(String value) {
    	m_ListeningAddress = value;
    }

    public int getPortRangeFrom() {
    	return m_PortRangeFrom;
    }
    
    public int getPortRangeTo() {
    	return m_PortRangeTo;
    }
    
    public String getPassiveModeHost()
    {
        return m_PassiveModeHost;
    }

    public void setPassiveModeHost(String value){
        m_PassiveModeHost = value;
    }

    public boolean getAllowAnonymous()
	{
		return m_AllowAnonymous;
	}

    public void setAllowAnonymous(boolean value) {
		m_AllowAnonymous = value;    	
    }
    
	public boolean getImplicitSSL()
	{
		return m_ImplicitSSL;
	}

	public void setImplicitSSL(boolean value) {
		m_ImplicitSSL = value;
	}
	
	public boolean getRequireTLS()
	{
		return m_RequireTLS;
	}

	public void setRequireTLS(boolean value){
		m_RequireTLS = value;	
	}
	
	public boolean getRequireTLSForData()
	{
		return m_RequireTLSForData;
	}

	public void setRequireTLSForData(boolean value) {
		m_RequireTLSForData = value;
	}
	
	public Certificate getServerCertificate()
	{
		return m_ServerCertificate;
	}

    private void GetParamAndValue(String Line, paramvalue result)
    {
        int i = Line.indexOf('=');
        if (i >= 0)
        {
            result.Param = Line.substring(0, i).trim().toLowerCase();
            result.Value = Line.substring(i + 1).trim();
        }
        else
        {
            result.Param = "";
            result.Value = "";
        }
    }

    private boolean BoolForYesNo(String Param)
    {
        return Param.toLowerCase().compareTo("yes") == 0;
    }

	public void Load()
	{
		try
		{
            /*
            Configuration file contains a sequence of:

            [Configuration]
            ImplicitSSL=yes
            RequireTLS=yes
            RequireTLSForData=yes
            ListenPort=22
            ListenAddress=
            CertificateFile=C:\Temp\cert.pfx
            CertificatePassword=password

             */

			String AppPath = new java.io.File("").getAbsolutePath();;
			AppPath = AppPath + "\\SecureBlackbox\\";
			File appDir = new File(AppPath);
			if (!appDir.exists()) appDir.mkdir();
            m_SettingsFile = AppPath + "FTPSSrvNETParams";

			if (!new File(m_SettingsFile).exists())
			{	
                m_SettingsFound = false;
				LoadDefaults();
                Save();
		   	}
			else
			{
                m_SettingsFound = true;
				String cfgLine = null;
                boolean InSettings = false;

    			FileInputStream fstream = new FileInputStream(m_SettingsFile);
    			DataInputStream in = new DataInputStream(fstream);
    			BufferedReader br = new BufferedReader(new InputStreamReader(in));
    	
    			while ((cfgLine = br.readLine()) != null) {
    				if (cfgLine.length() > 0) 
    				{
                        cfgLine = cfgLine.trim();

                        if (cfgLine.toLowerCase().compareTo("[configuration]") == 0)
                        {
                            InSettings = true;
                        }
                        else if (InSettings)
                        {
                        	paramvalue pv = new paramvalue();
                            GetParamAndValue(cfgLine, pv);

                            if (pv.Param.compareTo("allowanonymous") == 0)
                                setAllowAnonymous(BoolForYesNo(pv.Value));
                            if (pv.Param.compareTo("implicitssl") == 0)
                                setImplicitSSL(BoolForYesNo(pv.Value));
                            else if (pv.Param.compareTo("requiretls") == 0)
                            	setRequireTLS(BoolForYesNo(pv.Value));
                            else if (pv.Param.compareTo("requiretlsfordata") == 0)
                            	setRequireTLSForData(BoolForYesNo(pv.Value));
                            else if (pv.Param.compareTo("listeningport") == 0)
                            	setListeningPort(Integer.parseInt(pv.Value));
                            else if (pv.Param.compareTo("listeningaddress") == 0)
                            	setListeningAddress(pv.Value);
                            else if (pv.Param.compareTo("certificatefile") == 0)
                            	setCertificateFile(pv.Value);
                            else if (pv.Param.compareTo("certificatepassword") == 0)
                            	setCertificatePassword(pv.Value);
                            else if (pv.Param.compareTo("passivemodehost") == 0)
                            	setPassiveModeHost(pv.Value);
                            else if (pv.Param.compareTo("portrangefrom") == 0)
                                setPortRangeFrom(Integer.parseInt(pv.Value));
                            else if (pv.Param.compareTo("portrangeto") == 0)
                                setPortRangeTo(Integer.parseInt((pv.Value)));                            
                        }
    				}
    			}
    			
    			br.close();
    			in.close();
    			fstream.close();
   			}

            loadCertificate();
		}
		catch(Exception exc) 
		{
			exc.printStackTrace();
			ftpserver.Log("Error reading settings " + exc.getMessage(),true);
		}
	}

	public void setPortRangeTo(int value) {
		m_PortRangeTo = value;
	}
	
	public void setPortRangeFrom(int value) {
		m_PortRangeFrom = value;
	}
	
	/// <summary>
	/// Loads default settings
	/// </summary>
	public void LoadDefaults()
	{
		setListeningPort(21);
		setCertificateFile("");
		setCertificatePassword("");
		setImplicitSSL(false);
		setRequireTLS(false);
		setRequireTLSForData(false);
		setAllowAnonymous(true);

		Save();
	}

	/// <summary>
	/// Saves server configuration settings to files
	/// </summary>
    /// 

    public String YesNoForBool(boolean param)
    {
        return param ? "yes" : "no";
    }

	public void Save()
	{

		try
		{
			BufferedWriter sw = new BufferedWriter(new FileWriter(m_SettingsFile));


            sw.write("[Configuration]"); sw.newLine();
            sw.write("AllowAnonymous=" + YesNoForBool(getAllowAnonymous())); sw.newLine();
            sw.write("ImplicitSSL=" + YesNoForBool(getImplicitSSL())); sw.newLine();
            sw.write("RequireTLS=" + YesNoForBool(getRequireTLS())); sw.newLine();
            sw.write("RequireTLSForData=" + YesNoForBool(getRequireTLSForData())); sw.newLine();
            sw.write("ListeningPort=" + getListeningPort()); sw.newLine();
            sw.write("ListeningAddress=" + getListeningAddress()); sw.newLine();
            sw.write("PassiveModeHost=" + getPassiveModeHost()); sw.newLine();
            sw.write("CertificateFile=" + getCertificateFile()); sw.newLine();
            sw.write("CertificatePassword=" + getCertificatePassword()); sw.newLine();
            sw.write(""); sw.newLine();

			sw.close();
		}
		catch(Exception exc) 
		{
			exc.printStackTrace();
			ftpserver.Log("SaveSettings : " + exc.getMessage(), true);
		}
	}

    public void loadCertificate()
    {
		m_ServerCertificate = null;

		if (m_CertificateFile.length() > 0)
		{
			try
			{
				Certificatemanager manager = new Certificatemanager();
				manager.importFromFile(m_CertificateFile, m_CertificatePassword);
				m_ServerCertificate = manager.getCertificate();
			}
			catch (Exception exc)
			{
				ftpserver.showMessage("Cannot load FTPS server certificate!");
			}
		}
    }
}