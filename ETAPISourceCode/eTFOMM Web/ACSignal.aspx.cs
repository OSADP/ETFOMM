using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Data;
using System.ServiceModel;
using WCFServer;

public partial class _Default : System.Web.UI.Page
{
    //Connect WCF Service
    
    static EndpointAddress address;
    //static EndpointAddress address = new EndpointAddress("http://localhost:6000/service");
    static WSHttpBinding binding;
    static ChannelFactory<IService1> factory;
    IService1 proxy;


    enum ClientState
    {
        NotAvailable,
        Started,
        RequestingUpdate,
        NoRequest,
        APIShutDown,
        VehicleUpdateSubmitted,
        AddPathSubmitted,
        AddVehicleSubmitted,
        SignalsDataSubmitted,
        FixTimeControlSignalsDataSubmitted,
        Continued,
        Finished,
        UseHostDefineNetwork,
        UseClientDefineNetwork,
        UseTRFFile,
        NextRun,
        UpdateAll,
        UpdateFVehicle,
        UpdateSVehicle,
        UpdateFreewayLink,
        UpdateStreetLink,
        UpdateFTCSignal,
        UpdateACSignal,
        UpdateEntryNode,
        UpdateRampMeter,
        UpdateNInputs,
        UpdateFNInputs,
        UpdateSNInputs,
        UpdateVTypes,
        UpdateFDetInputs,
        UpdateSDetInputs,
        UpdateCPTInputs,
        UpdateBRInputs,
        UpdateBSInputs,
        UpdateIncidents,
        UpdateXYCoords,
        UpdateParkZInputs,
        UpdateEventInputs,
        UpdateDiverInputs,
        DataReady,
        GetCoordinationData,
        GotCoordinationData,
        SetCoordinationData,
        SetCoordinationDataDone,
        SetNewCycleLength,
        GetNewCycleLength,
        GetCycleLength,
        SetNewOffset,
        GetNewOffset,
        GetOffset,
        GetLocalCycleTimer,
        SetNewSplits,
        GetNewSplits,
        GetSplits,
        GetMinSplits,
        GetAverageTravelTime,
        GotAverageTravelTime,
        GetStreetLaneMOEData,
        GotStreetLaneMOEData,
        GetStreetLinkMOEData,
        GotStreetLinkMOEData,
        GetNodeGYR,
        SetNodeGYR
    };

    
    protected void Page_Load(object sender, EventArgs e)
    {
        address = new EndpointAddress("http://localhost:8000/service");
        binding = new WSHttpBinding();
        binding.MaxBufferPoolSize = 2147483647;
        binding.MaxReceivedMessageSize = 2147483647; 
        binding.Security.Mode = SecurityMode.None;
        factory = new ChannelFactory<IService1>(binding, address);
        proxy = factory.CreateChannel();
    }

    protected void End_Click(object sender, EventArgs e)
    {
        proxy.SetClientState((int)ClientState.NoRequest);
    }

    protected void TimeButton_Click(object sender, EventArgs e)
    {
        String timeinterval = TextBoxTimestep.Text;
        int ti = Convert.ToInt32(timeinterval);
        proxy.SetAPITimestepInterval(ti);
    }

    protected void Pass_Click(object sender, EventArgs e)
    {
        proxy.SetClientState((int)ClientState.NoRequest);
    }
    protected void ButtonStartSim_Click(object sender, EventArgs e)
    {
        proxy.SetNumberOfConnectedClients(1);
        proxy.SetAPITimestepInterval(60);
        proxy.SetServerWriteTextFlag(0);
        proxy.SetServerTRFFile("C:\\etRunnerWeb\\Sample\\Starkville182DemoS3v2.trf");
        proxy.SetClientState((int)ClientState.UseTRFFile);
        ShowNextTimeStep();
        PopulateACNodeList();
        ShowACData();
    }
    protected void ButtonStepSim_Click(object sender, EventArgs e)
    {
        proxy.SetClientState((int)ClientState.Continued);
        ShowNextTimeStep();
        ShowACData();
    }
    protected void TextBoxTimestep_TextChanged(object sender, EventArgs e)
    {

    }
    protected void ButtonEndSim_Click(object sender, EventArgs e)
    {
        proxy.SetClientState((int)ClientState.APIShutDown);
    }

    protected void ShowNextTimeStep()
    {
        int state = proxy.GetClientState();
        while (state != (int)ClientState.DataReady)
        {
            state = proxy.GetClientState();
        }
        float nextTimeStep = proxy.GetServerTimestep() + 1;
        TextBoxCurTimeStep.Text = nextTimeStep.ToString();
    }

    private void PopulateACNodeList()
    {
        WCF_AC[] acs = new WCF_AC[1];
        acs = proxy.GetServerACData();
        
        for (int i = 0; i < acs.Length; ++i)
        {
            DropDownListNodeID.Items.Add(new ListItem(acs[i].node.ToString(), (i+1).ToString()));
        }
    }

    private void ShowACData()
    {
        string strNID = DropDownListNodeID.SelectedItem.Text;
        int NodeID = Convert.ToInt32(strNID);
        WCF_COORDINATION_DATA[] wcf_coordination_data = new WCF_COORDINATION_DATA[1];
        wcf_coordination_data[0].node = NodeID;
        proxy.SetServerCoordinationData(wcf_coordination_data);
        proxy.SetClientState((int)ClientState.GetCoordinationData);

        while (proxy.GetClientState() != (int)ClientState.GotCoordinationData)
        {
        }
        wcf_coordination_data = proxy.GetServerCoordinationData();
        TextBoxCLength.Text = wcf_coordination_data[0].cycle_length.ToString();
        TextBoxOffset.Text = wcf_coordination_data[0].offset.ToString();
        TextBoxSplit1.Text = wcf_coordination_data[0].splits[0].ToString();
        TextBoxSplit2.Text = wcf_coordination_data[0].splits[1].ToString();
        TextBoxSplit3.Text = wcf_coordination_data[0].splits[2].ToString();
        TextBoxSplit4.Text = wcf_coordination_data[0].splits[3].ToString();
        TextBoxSplit5.Text = wcf_coordination_data[0].splits[4].ToString();
        TextBoxSplit6.Text = wcf_coordination_data[0].splits[5].ToString();
        TextBoxSplit7.Text = wcf_coordination_data[0].splits[6].ToString();
        TextBoxSplit8.Text = wcf_coordination_data[0].splits[7].ToString();
    }

    protected void ButtonGetCLength_Click(object sender, EventArgs e)
    {
        string strNID = DropDownListNodeID.SelectedItem.Text;
        int node_id = Convert.ToInt32(strNID);
        proxy.SetServerNodeID(node_id);
        proxy.SetClientState((int)ClientState.GetCycleLength);
        while (proxy.GetClientState() != (int)ClientState.GotCoordinationData)
        {
        }
        float cycle_length = proxy.GetServerCycleLength();
        TextBoxCLength.Text = cycle_length.ToString();
    }
   
    protected void ButtonSetNewCLength_Click(object sender, EventArgs e)
    {
        string strNID = DropDownListNodeID.SelectedItem.Text;
        int node_id = Convert.ToInt32(strNID);
        float new_cycle_length = (float)(Convert.ToDouble(TextBoxNewCLength.Text));
        proxy.SetServerNewCycleLength(node_id, new_cycle_length);
        proxy.SetClientState((int)ClientState.SetNewCycleLength);
        while (proxy.GetClientState() != (int)ClientState.SetCoordinationDataDone)
        {
        }
    }

    protected void ButtonGetNewCLength_Click(object sender, EventArgs e)
    {
        string strNID = DropDownListNodeID.SelectedItem.Text;
        int node_id = Convert.ToInt32(strNID);
        proxy.SetServerNodeID(node_id);
        proxy.SetClientState((int)ClientState.GetNewCycleLength);
        while (proxy.GetClientState() != (int)ClientState.GotCoordinationData)
        {
        }
        float new_cycle_length = proxy.GetServerNewCycleLength();
        TextBoxNewCLength.Text = new_cycle_length.ToString();
    }

    protected void ButtonGetOffset_Click(object sender, EventArgs e)
    {
        string strNID = DropDownListNodeID.SelectedItem.Text;
        int node_id = Convert.ToInt32(strNID);
        proxy.SetServerNodeID(node_id);
        proxy.SetClientState((int)ClientState.GetOffset);
        while (proxy.GetClientState() != (int)ClientState.GotCoordinationData)
        {
        }
        float offset = proxy.GetServerOffset();
        TextBoxOffset.Text = offset.ToString();
    }
    protected void ButtonSetNewOffset_Click(object sender, EventArgs e)
    {
        string strNID = DropDownListNodeID.SelectedItem.Text;
        int node_id = Convert.ToInt32(strNID);
        float new_offset = (float)(Convert.ToDouble(TextBoxNewOffset.Text));
        proxy.SetServerNewOffset(node_id, new_offset);
        proxy.SetClientState((int)ClientState.SetNewOffset);
        while (proxy.GetClientState() != (int)ClientState.SetCoordinationDataDone)
        {
        }
    }
    protected void ButtonGetNewOffset_Click(object sender, EventArgs e)
    {
        string strNID = DropDownListNodeID.SelectedItem.Text;
        int node_id = Convert.ToInt32(strNID);
        proxy.SetServerNodeID(node_id);
        proxy.SetClientState((int)ClientState.GetNewOffset);
        while (proxy.GetClientState() != (int)ClientState.GotCoordinationData)
        {
        }
        float new_offset = proxy.GetServerNewOffset();
        TextBoxNewOffset.Text = new_offset.ToString();
    }
    protected void ButtonGetSplits_Click(object sender, EventArgs e)
    {
        string strNID = DropDownListNodeID.SelectedItem.Text;
        int node_id = Convert.ToInt32(strNID);
        proxy.SetServerNodeID(node_id);
		proxy.SetClientState((int)ClientState.GetSplits);

		while (proxy.GetClientState() != (int)ClientState.GotCoordinationData)
		{
		}
        float[] splits = new float[8];
		splits = proxy.GetServerSplits();
        TextBoxSplit1.Text = splits[0].ToString();
        TextBoxSplit2.Text = splits[1].ToString();
        TextBoxSplit3.Text = splits[2].ToString();
        TextBoxSplit4.Text = splits[3].ToString();
        TextBoxSplit5.Text = splits[4].ToString();
        TextBoxSplit6.Text = splits[5].ToString();
        TextBoxSplit7.Text = splits[6].ToString();
        TextBoxSplit8.Text = splits[7].ToString();
        
    }
    protected void ButtonSetNewSplits_Click(object sender, EventArgs e)
    {
        string strNID = DropDownListNodeID.SelectedItem.Text;
        int node_id = Convert.ToInt32(strNID);
        float[] splits = new float[8];
        splits[0] = (float)(Convert.ToDouble(TextBoxNewSplit1.Text));
        splits[1] = (float)(Convert.ToDouble(TextBoxNewSplit2.Text));
        splits[2] = (float)(Convert.ToDouble(TextBoxNewSplit3.Text));
        splits[3] = (float)(Convert.ToDouble(TextBoxNewSplit4.Text));
        splits[4] = (float)(Convert.ToDouble(TextBoxNewSplit5.Text));
        splits[5] = (float)(Convert.ToDouble(TextBoxNewSplit6.Text));
        splits[6] = (float)(Convert.ToDouble(TextBoxNewSplit7.Text));
        splits[7] = (float)(Convert.ToDouble(TextBoxNewSplit8.Text));
        proxy.SetServerNewSplits(node_id, splits);
        proxy.SetClientState((int)ClientState.SetNewSplits);
        while (proxy.GetClientState() != (int)ClientState.SetCoordinationDataDone)
        {
        }
    }
    protected void ButtonGetNewSplits_Click(object sender, EventArgs e)
    {
        string strNID = DropDownListNodeID.SelectedItem.Text;
        int node_id = Convert.ToInt32(strNID);
        proxy.SetServerNodeID(node_id);
		proxy.SetClientState((int)ClientState.GetNewSplits);

		while (proxy.GetClientState() != (int)ClientState.GotCoordinationData)
		{
		}
        float[] splits = new float[8];
		splits = proxy.GetServerNewSplits();
        TextBoxNewSplit1.Text = splits[0].ToString();
        TextBoxNewSplit2.Text = splits[1].ToString();
        TextBoxNewSplit3.Text = splits[2].ToString();
        TextBoxNewSplit4.Text = splits[3].ToString();
        TextBoxNewSplit5.Text = splits[4].ToString();
        TextBoxNewSplit6.Text = splits[5].ToString();
        TextBoxNewSplit7.Text = splits[6].ToString();
        TextBoxNewSplit8.Text = splits[7].ToString();
    }
    protected void ButtonGetACData_Click(object sender, EventArgs e)
    {
        ShowACData();
    }
}
