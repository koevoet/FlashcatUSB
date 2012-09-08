<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class MainForm
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(MainForm))
        Me.ContextMenuStrip1 = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.MenuStrip1 = New System.Windows.Forms.MenuStrip()
        Me.MainToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.miDetectDevice = New System.Windows.Forms.ToolStripMenuItem()
        Me.tsiBootloader = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator()
        Me.ExitToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SettingsToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.VerifyMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.EnableJTAGVccPinToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SPINRF24LE1ModeToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ScriptToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CurrentScript_MI = New System.Windows.Forms.ToolStripMenuItem()
        Me.LoadScript_MI = New System.Windows.Forms.ToolStripMenuItem()
        Me.UnloadScript_MI = New System.Windows.Forms.ToolStripMenuItem()
        Me.LanguageToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.EnglishToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ChineseToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.FrenchToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.GermanToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.PortugueseToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SpanishToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.MyTabs = New System.Windows.Forms.TabControl()
        Me.TabStatus = New System.Windows.Forms.TabPage()
        Me.TableLayoutPanel2 = New System.Windows.Forms.TableLayoutPanel()
        Me.sm7 = New System.Windows.Forms.Label()
        Me.sm6 = New System.Windows.Forms.Label()
        Me.sm5 = New System.Windows.Forms.Label()
        Me.sm4 = New System.Windows.Forms.Label()
        Me.sm3 = New System.Windows.Forms.Label()
        Me.sm2 = New System.Windows.Forms.Label()
        Me.lblStatus = New System.Windows.Forms.Label()
        Me.sm1 = New System.Windows.Forms.Label()
        Me.TableLayoutPanel1 = New System.Windows.Forms.TableLayoutPanel()
        Me.PictureBox1 = New System.Windows.Forms.PictureBox()
        Me.TabConsole = New System.Windows.Forms.TabPage()
        Me.cmdSaveLog = New System.Windows.Forms.Button()
        Me.txtInput = New System.Windows.Forms.TextBox()
        Me.ConsoleBox = New System.Windows.Forms.ListBox()
        Me.AvrTab = New System.Windows.Forms.TabPage()
        Me.AvrEditor = New FlashcatUSB.HexEditor()
        Me.lblAvrCrc = New System.Windows.Forms.Label()
        Me.lblAvrRange = New System.Windows.Forms.Label()
        Me.lblAvrFn = New System.Windows.Forms.Label()
        Me.cmdAvrProg = New System.Windows.Forms.Button()
        Me.cmdAvrStart = New System.Windows.Forms.Button()
        Me.cmdAvrLoad = New System.Windows.Forms.Button()
        Me.DfuPbBar = New System.Windows.Forms.ProgressBar()
        Me.SpiTab = New System.Windows.Forms.TabPage()
        Me.cbSPI = New System.Windows.Forms.GroupBox()
        Me.cbSpiProgMode = New System.Windows.Forms.ComboBox()
        Me.cbUseEnWS = New System.Windows.Forms.CheckBox()
        Me.txtEnWS = New System.Windows.Forms.TextBox()
        Me.lblSpiEnWrStatus = New System.Windows.Forms.Label()
        Me.lblSpiProgMode = New System.Windows.Forms.Label()
        Me.lblSpiInfo = New System.Windows.Forms.Label()
        Me.lblSpiClockDiv = New System.Windows.Forms.Label()
        Me.lblSpiMode = New System.Windows.Forms.Label()
        Me.lblSpiBitOrder = New System.Windows.Forms.Label()
        Me.txtChipSize = New System.Windows.Forms.TextBox()
        Me.lblSpiChipSize = New System.Windows.Forms.Label()
        Me.cbBitOrder = New System.Windows.Forms.ComboBox()
        Me.cbSpiClock = New System.Windows.Forms.ComboBox()
        Me.cbSpiMode = New System.Windows.Forms.ComboBox()
        Me.txtChipErase = New System.Windows.Forms.TextBox()
        Me.lblSpiRead = New System.Windows.Forms.Label()
        Me.txtRead = New System.Windows.Forms.TextBox()
        Me.txtWriteStatus = New System.Windows.Forms.TextBox()
        Me.lblSpiSectorErase = New System.Windows.Forms.Label()
        Me.lblSpiWriteStatus = New System.Windows.Forms.Label()
        Me.txtSectorErase = New System.Windows.Forms.TextBox()
        Me.txtReadStatus = New System.Windows.Forms.TextBox()
        Me.lblSpiEraseSize = New System.Windows.Forms.Label()
        Me.lblSpiReadStatus = New System.Windows.Forms.Label()
        Me.txtEraseSize = New System.Windows.Forms.TextBox()
        Me.txtWriteEnable = New System.Windows.Forms.TextBox()
        Me.lblSpiWriteEn = New System.Windows.Forms.Label()
        Me.txtPageProgram = New System.Windows.Forms.TextBox()
        Me.lblSpiChipErase = New System.Windows.Forms.Label()
        Me.lblSpiPageProgram = New System.Windows.Forms.Label()
        Me.RadioUseSpiSettings = New System.Windows.Forms.RadioButton()
        Me.RadioUseSpiAuto = New System.Windows.Forms.RadioButton()
        Me.StatusStrip1 = New System.Windows.Forms.StatusStrip()
        Me.Status = New System.Windows.Forms.ToolStripStatusLabel()
        Me.MenuStrip1.SuspendLayout()
        Me.MyTabs.SuspendLayout()
        Me.TabStatus.SuspendLayout()
        Me.TableLayoutPanel2.SuspendLayout()
        Me.TableLayoutPanel1.SuspendLayout()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabConsole.SuspendLayout()
        Me.AvrTab.SuspendLayout()
        Me.SpiTab.SuspendLayout()
        Me.cbSPI.SuspendLayout()
        Me.StatusStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'ContextMenuStrip1
        '
        Me.ContextMenuStrip1.Name = "ContextMenuStrip1"
        Me.ContextMenuStrip1.Size = New System.Drawing.Size(61, 4)
        '
        'MenuStrip1
        '
        Me.MenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.MainToolStripMenuItem, Me.SettingsToolStripMenuItem, Me.ScriptToolStripMenuItem, Me.LanguageToolStripMenuItem})
        Me.MenuStrip1.Location = New System.Drawing.Point(0, 0)
        Me.MenuStrip1.Name = "MenuStrip1"
        Me.MenuStrip1.Size = New System.Drawing.Size(452, 24)
        Me.MenuStrip1.TabIndex = 1
        Me.MenuStrip1.Text = "MenuStrip1"
        '
        'MainToolStripMenuItem
        '
        Me.MainToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.miDetectDevice, Me.tsiBootloader, Me.ToolStripSeparator1, Me.ExitToolStripMenuItem})
        Me.MainToolStripMenuItem.Name = "MainToolStripMenuItem"
        Me.MainToolStripMenuItem.Size = New System.Drawing.Size(41, 20)
        Me.MainToolStripMenuItem.Text = "Main"
        '
        'miDetectDevice
        '
        Me.miDetectDevice.Image = Global.FlashcatUSB.My.Resources.Resources.upgrade
        Me.miDetectDevice.Name = "miDetectDevice"
        Me.miDetectDevice.ShortcutKeys = System.Windows.Forms.Keys.F1
        Me.miDetectDevice.Size = New System.Drawing.Size(159, 22)
        Me.miDetectDevice.Text = "Detect device"
        '
        'tsiBootloader
        '
        Me.tsiBootloader.Name = "tsiBootloader"
        Me.tsiBootloader.Size = New System.Drawing.Size(159, 22)
        Me.tsiBootloader.Text = "Enter Bootloader"
        Me.tsiBootloader.Visible = False
        '
        'ToolStripSeparator1
        '
        Me.ToolStripSeparator1.Name = "ToolStripSeparator1"
        Me.ToolStripSeparator1.Size = New System.Drawing.Size(156, 6)
        '
        'ExitToolStripMenuItem
        '
        Me.ExitToolStripMenuItem.Image = Global.FlashcatUSB.My.Resources.Resources.ico_exit
        Me.ExitToolStripMenuItem.Name = "ExitToolStripMenuItem"
        Me.ExitToolStripMenuItem.Size = New System.Drawing.Size(159, 22)
        Me.ExitToolStripMenuItem.Text = "Exit"
        '
        'SettingsToolStripMenuItem
        '
        Me.SettingsToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.VerifyMenuItem, Me.EnableJTAGVccPinToolStripMenuItem, Me.SPINRF24LE1ModeToolStripMenuItem})
        Me.SettingsToolStripMenuItem.Name = "SettingsToolStripMenuItem"
        Me.SettingsToolStripMenuItem.Size = New System.Drawing.Size(58, 20)
        Me.SettingsToolStripMenuItem.Text = "Settings"
        '
        'VerifyMenuItem
        '
        Me.VerifyMenuItem.Name = "VerifyMenuItem"
        Me.VerifyMenuItem.Size = New System.Drawing.Size(174, 22)
        Me.VerifyMenuItem.Text = "Verify programming"
        '
        'EnableJTAGVccPinToolStripMenuItem
        '
        Me.EnableJTAGVccPinToolStripMenuItem.Name = "EnableJTAGVccPinToolStripMenuItem"
        Me.EnableJTAGVccPinToolStripMenuItem.Size = New System.Drawing.Size(174, 22)
        Me.EnableJTAGVccPinToolStripMenuItem.Text = "Enable JTAG VCC Pin"
        '
        'SPINRF24LE1ModeToolStripMenuItem
        '
        Me.SPINRF24LE1ModeToolStripMenuItem.Name = "SPINRF24LE1ModeToolStripMenuItem"
        Me.SPINRF24LE1ModeToolStripMenuItem.Size = New System.Drawing.Size(174, 22)
        Me.SPINRF24LE1ModeToolStripMenuItem.Text = "SPI nRF24LE1 mode"
        '
        'ScriptToolStripMenuItem
        '
        Me.ScriptToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.CurrentScript_MI, Me.LoadScript_MI, Me.UnloadScript_MI})
        Me.ScriptToolStripMenuItem.Name = "ScriptToolStripMenuItem"
        Me.ScriptToolStripMenuItem.Size = New System.Drawing.Size(46, 20)
        Me.ScriptToolStripMenuItem.Text = "Script"
        '
        'CurrentScript_MI
        '
        Me.CurrentScript_MI.Image = Global.FlashcatUSB.My.Resources.Resources.config
        Me.CurrentScript_MI.Name = "CurrentScript_MI"
        Me.CurrentScript_MI.Size = New System.Drawing.Size(140, 22)
        Me.CurrentScript_MI.Text = "Current script"
        '
        'LoadScript_MI
        '
        Me.LoadScript_MI.Image = Global.FlashcatUSB.My.Resources.Resources.openfile
        Me.LoadScript_MI.Name = "LoadScript_MI"
        Me.LoadScript_MI.Size = New System.Drawing.Size(140, 22)
        Me.LoadScript_MI.Text = "Load script"
        '
        'UnloadScript_MI
        '
        Me.UnloadScript_MI.Image = Global.FlashcatUSB.My.Resources.Resources.clear_x
        Me.UnloadScript_MI.Name = "UnloadScript_MI"
        Me.UnloadScript_MI.Size = New System.Drawing.Size(140, 22)
        Me.UnloadScript_MI.Text = "Unload script"
        '
        'LanguageToolStripMenuItem
        '
        Me.LanguageToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.EnglishToolStripMenuItem, Me.ChineseToolStripMenuItem, Me.FrenchToolStripMenuItem, Me.GermanToolStripMenuItem, Me.PortugueseToolStripMenuItem, Me.SpanishToolStripMenuItem})
        Me.LanguageToolStripMenuItem.Name = "LanguageToolStripMenuItem"
        Me.LanguageToolStripMenuItem.Size = New System.Drawing.Size(66, 20)
        Me.LanguageToolStripMenuItem.Text = "Language"
        '
        'EnglishToolStripMenuItem
        '
        Me.EnglishToolStripMenuItem.Image = Global.FlashcatUSB.My.Resources.Resources.English
        Me.EnglishToolStripMenuItem.Name = "EnglishToolStripMenuItem"
        Me.EnglishToolStripMenuItem.Size = New System.Drawing.Size(129, 22)
        Me.EnglishToolStripMenuItem.Text = "English"
        '
        'ChineseToolStripMenuItem
        '
        Me.ChineseToolStripMenuItem.Image = Global.FlashcatUSB.My.Resources.Resources.china
        Me.ChineseToolStripMenuItem.Name = "ChineseToolStripMenuItem"
        Me.ChineseToolStripMenuItem.Size = New System.Drawing.Size(129, 22)
        Me.ChineseToolStripMenuItem.Text = "Chinese"
        Me.ChineseToolStripMenuItem.Visible = False
        '
        'FrenchToolStripMenuItem
        '
        Me.FrenchToolStripMenuItem.Image = Global.FlashcatUSB.My.Resources.Resources.france
        Me.FrenchToolStripMenuItem.Name = "FrenchToolStripMenuItem"
        Me.FrenchToolStripMenuItem.Size = New System.Drawing.Size(129, 22)
        Me.FrenchToolStripMenuItem.Text = "French"
        '
        'GermanToolStripMenuItem
        '
        Me.GermanToolStripMenuItem.Image = Global.FlashcatUSB.My.Resources.Resources.german
        Me.GermanToolStripMenuItem.Name = "GermanToolStripMenuItem"
        Me.GermanToolStripMenuItem.Size = New System.Drawing.Size(129, 22)
        Me.GermanToolStripMenuItem.Text = "German"
        '
        'PortugueseToolStripMenuItem
        '
        Me.PortugueseToolStripMenuItem.Image = Global.FlashcatUSB.My.Resources.Resources.portugal
        Me.PortugueseToolStripMenuItem.Name = "PortugueseToolStripMenuItem"
        Me.PortugueseToolStripMenuItem.Size = New System.Drawing.Size(129, 22)
        Me.PortugueseToolStripMenuItem.Text = "Portuguese"
        '
        'SpanishToolStripMenuItem
        '
        Me.SpanishToolStripMenuItem.Image = Global.FlashcatUSB.My.Resources.Resources.spain
        Me.SpanishToolStripMenuItem.Name = "SpanishToolStripMenuItem"
        Me.SpanishToolStripMenuItem.Size = New System.Drawing.Size(129, 22)
        Me.SpanishToolStripMenuItem.Text = "Spanish"
        '
        'MyTabs
        '
        Me.MyTabs.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.MyTabs.Controls.Add(Me.TabStatus)
        Me.MyTabs.Controls.Add(Me.TabConsole)
        Me.MyTabs.Controls.Add(Me.AvrTab)
        Me.MyTabs.Controls.Add(Me.SpiTab)
        Me.MyTabs.Location = New System.Drawing.Point(0, 25)
        Me.MyTabs.Name = "MyTabs"
        Me.MyTabs.SelectedIndex = 0
        Me.MyTabs.Size = New System.Drawing.Size(452, 315)
        Me.MyTabs.TabIndex = 2
        '
        'TabStatus
        '
        Me.TabStatus.Controls.Add(Me.TableLayoutPanel2)
        Me.TabStatus.Controls.Add(Me.TableLayoutPanel1)
        Me.TabStatus.Location = New System.Drawing.Point(4, 22)
        Me.TabStatus.Name = "TabStatus"
        Me.TabStatus.Padding = New System.Windows.Forms.Padding(3)
        Me.TabStatus.Size = New System.Drawing.Size(444, 289)
        Me.TabStatus.TabIndex = 0
        Me.TabStatus.Text = "Status"
        Me.TabStatus.UseVisualStyleBackColor = True
        '
        'TableLayoutPanel2
        '
        Me.TableLayoutPanel2.ColumnCount = 1
        Me.TableLayoutPanel2.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle())
        Me.TableLayoutPanel2.Controls.Add(Me.sm7, 0, 7)
        Me.TableLayoutPanel2.Controls.Add(Me.sm6, 0, 6)
        Me.TableLayoutPanel2.Controls.Add(Me.sm5, 0, 5)
        Me.TableLayoutPanel2.Controls.Add(Me.sm4, 0, 4)
        Me.TableLayoutPanel2.Controls.Add(Me.sm3, 0, 3)
        Me.TableLayoutPanel2.Controls.Add(Me.sm2, 0, 2)
        Me.TableLayoutPanel2.Controls.Add(Me.lblStatus, 0, 0)
        Me.TableLayoutPanel2.Controls.Add(Me.sm1, 0, 1)
        Me.TableLayoutPanel2.Location = New System.Drawing.Point(3, 12)
        Me.TableLayoutPanel2.Name = "TableLayoutPanel2"
        Me.TableLayoutPanel2.RowCount = 9
        Me.TableLayoutPanel2.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20.0!))
        Me.TableLayoutPanel2.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20.0!))
        Me.TableLayoutPanel2.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20.0!))
        Me.TableLayoutPanel2.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20.0!))
        Me.TableLayoutPanel2.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20.0!))
        Me.TableLayoutPanel2.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20.0!))
        Me.TableLayoutPanel2.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20.0!))
        Me.TableLayoutPanel2.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20.0!))
        Me.TableLayoutPanel2.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20.0!))
        Me.TableLayoutPanel2.Size = New System.Drawing.Size(438, 165)
        Me.TableLayoutPanel2.TabIndex = 4
        '
        'sm7
        '
        Me.sm7.AutoSize = True
        Me.sm7.Location = New System.Drawing.Point(3, 140)
        Me.sm7.Name = "sm7"
        Me.sm7.Size = New System.Drawing.Size(0, 13)
        Me.sm7.TabIndex = 10
        '
        'sm6
        '
        Me.sm6.AutoSize = True
        Me.sm6.Location = New System.Drawing.Point(3, 120)
        Me.sm6.Name = "sm6"
        Me.sm6.Size = New System.Drawing.Size(0, 13)
        Me.sm6.TabIndex = 9
        '
        'sm5
        '
        Me.sm5.AutoSize = True
        Me.sm5.Location = New System.Drawing.Point(3, 100)
        Me.sm5.Name = "sm5"
        Me.sm5.Size = New System.Drawing.Size(0, 13)
        Me.sm5.TabIndex = 8
        '
        'sm4
        '
        Me.sm4.AutoSize = True
        Me.sm4.Location = New System.Drawing.Point(3, 80)
        Me.sm4.Name = "sm4"
        Me.sm4.Size = New System.Drawing.Size(0, 13)
        Me.sm4.TabIndex = 7
        '
        'sm3
        '
        Me.sm3.AutoSize = True
        Me.sm3.Location = New System.Drawing.Point(3, 60)
        Me.sm3.Name = "sm3"
        Me.sm3.Size = New System.Drawing.Size(0, 13)
        Me.sm3.TabIndex = 6
        '
        'sm2
        '
        Me.sm2.AutoSize = True
        Me.sm2.Location = New System.Drawing.Point(3, 40)
        Me.sm2.Name = "sm2"
        Me.sm2.Size = New System.Drawing.Size(0, 13)
        Me.sm2.TabIndex = 5
        '
        'lblStatus
        '
        Me.lblStatus.AutoSize = True
        Me.lblStatus.Location = New System.Drawing.Point(3, 0)
        Me.lblStatus.Name = "lblStatus"
        Me.lblStatus.Size = New System.Drawing.Size(177, 13)
        Me.lblStatus.TabIndex = 3
        Me.lblStatus.Text = "FlashcatUSB status: Not connected"
        '
        'sm1
        '
        Me.sm1.AutoSize = True
        Me.sm1.Location = New System.Drawing.Point(3, 20)
        Me.sm1.Name = "sm1"
        Me.sm1.Size = New System.Drawing.Size(0, 13)
        Me.sm1.TabIndex = 4
        '
        'TableLayoutPanel1
        '
        Me.TableLayoutPanel1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.TableLayoutPanel1.ColumnCount = 3
        Me.TableLayoutPanel1.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 50.0!))
        Me.TableLayoutPanel1.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 240.0!))
        Me.TableLayoutPanel1.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 50.0!))
        Me.TableLayoutPanel1.Controls.Add(Me.PictureBox1, 1, 0)
        Me.TableLayoutPanel1.Location = New System.Drawing.Point(3, 184)
        Me.TableLayoutPanel1.Name = "TableLayoutPanel1"
        Me.TableLayoutPanel1.RowCount = 1
        Me.TableLayoutPanel1.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 100.0!))
        Me.TableLayoutPanel1.Size = New System.Drawing.Size(438, 102)
        Me.TableLayoutPanel1.TabIndex = 2
        '
        'PictureBox1
        '
        Me.PictureBox1.BackgroundImageLayout = System.Windows.Forms.ImageLayout.None
        Me.PictureBox1.Image = CType(resources.GetObject("PictureBox1.Image"), System.Drawing.Image)
        Me.PictureBox1.Location = New System.Drawing.Point(102, 3)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(234, 95)
        Me.PictureBox1.TabIndex = 0
        Me.PictureBox1.TabStop = False
        '
        'TabConsole
        '
        Me.TabConsole.Controls.Add(Me.cmdSaveLog)
        Me.TabConsole.Controls.Add(Me.txtInput)
        Me.TabConsole.Controls.Add(Me.ConsoleBox)
        Me.TabConsole.Location = New System.Drawing.Point(4, 22)
        Me.TabConsole.Name = "TabConsole"
        Me.TabConsole.Padding = New System.Windows.Forms.Padding(3)
        Me.TabConsole.Size = New System.Drawing.Size(444, 289)
        Me.TabConsole.TabIndex = 1
        Me.TabConsole.Text = "Console"
        Me.TabConsole.UseVisualStyleBackColor = True
        '
        'cmdSaveLog
        '
        Me.cmdSaveLog.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cmdSaveLog.FlatStyle = System.Windows.Forms.FlatStyle.Popup
        Me.cmdSaveLog.Image = CType(resources.GetObject("cmdSaveLog.Image"), System.Drawing.Image)
        Me.cmdSaveLog.Location = New System.Drawing.Point(416, 260)
        Me.cmdSaveLog.Name = "cmdSaveLog"
        Me.cmdSaveLog.Size = New System.Drawing.Size(22, 22)
        Me.cmdSaveLog.TabIndex = 5
        Me.cmdSaveLog.UseVisualStyleBackColor = True
        '
        'txtInput
        '
        Me.txtInput.Anchor = CType(((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.txtInput.Location = New System.Drawing.Point(3, 260)
        Me.txtInput.Name = "txtInput"
        Me.txtInput.Size = New System.Drawing.Size(410, 20)
        Me.txtInput.TabIndex = 4
        '
        'ConsoleBox
        '
        Me.ConsoleBox.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.ConsoleBox.FormattingEnabled = True
        Me.ConsoleBox.Location = New System.Drawing.Point(3, 3)
        Me.ConsoleBox.Name = "ConsoleBox"
        Me.ConsoleBox.Size = New System.Drawing.Size(438, 251)
        Me.ConsoleBox.TabIndex = 0
        '
        'AvrTab
        '
        Me.AvrTab.Controls.Add(Me.AvrEditor)
        Me.AvrTab.Controls.Add(Me.lblAvrCrc)
        Me.AvrTab.Controls.Add(Me.lblAvrRange)
        Me.AvrTab.Controls.Add(Me.lblAvrFn)
        Me.AvrTab.Controls.Add(Me.cmdAvrProg)
        Me.AvrTab.Controls.Add(Me.cmdAvrStart)
        Me.AvrTab.Controls.Add(Me.cmdAvrLoad)
        Me.AvrTab.Controls.Add(Me.DfuPbBar)
        Me.AvrTab.Location = New System.Drawing.Point(4, 22)
        Me.AvrTab.Name = "AvrTab"
        Me.AvrTab.Padding = New System.Windows.Forms.Padding(3)
        Me.AvrTab.Size = New System.Drawing.Size(444, 289)
        Me.AvrTab.TabIndex = 2
        Me.AvrTab.Text = "AVR Firmware"
        Me.AvrTab.UseVisualStyleBackColor = True
        '
        'AvrEditor
        '
        Me.AvrEditor.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.AvrEditor.Location = New System.Drawing.Point(6, 50)
        Me.AvrEditor.Name = "AvrEditor"
        Me.AvrEditor.Size = New System.Drawing.Size(435, 217)
        Me.AvrEditor.TabIndex = 15
        '
        'lblAvrCrc
        '
        Me.lblAvrCrc.AutoSize = True
        Me.lblAvrCrc.ForeColor = System.Drawing.Color.Gray
        Me.lblAvrCrc.Location = New System.Drawing.Point(354, 6)
        Me.lblAvrCrc.Name = "lblAvrCrc"
        Me.lblAvrCrc.Size = New System.Drawing.Size(82, 13)
        Me.lblAvrCrc.TabIndex = 14
        Me.lblAvrCrc.Text = "CRC: 0x000000"
        '
        'lblAvrRange
        '
        Me.lblAvrRange.AutoSize = True
        Me.lblAvrRange.ForeColor = System.Drawing.Color.Gray
        Me.lblAvrRange.Location = New System.Drawing.Point(224, 6)
        Me.lblAvrRange.Name = "lblAvrRange"
        Me.lblAvrRange.Size = New System.Drawing.Size(124, 13)
        Me.lblAvrRange.TabIndex = 13
        Me.lblAvrRange.Text = "Range: 0x0000 - 0x0000"
        '
        'lblAvrFn
        '
        Me.lblAvrFn.AutoSize = True
        Me.lblAvrFn.Location = New System.Drawing.Point(8, 6)
        Me.lblAvrFn.Name = "lblAvrFn"
        Me.lblAvrFn.Size = New System.Drawing.Size(135, 13)
        Me.lblAvrFn.TabIndex = 12
        Me.lblAvrFn.Text = "File: no file currently loaded"
        '
        'cmdAvrProg
        '
        Me.cmdAvrProg.Location = New System.Drawing.Point(156, 24)
        Me.cmdAvrProg.Name = "cmdAvrProg"
        Me.cmdAvrProg.Size = New System.Drawing.Size(112, 22)
        Me.cmdAvrProg.TabIndex = 9
        Me.cmdAvrProg.Text = "Program"
        Me.cmdAvrProg.UseVisualStyleBackColor = True
        '
        'cmdAvrStart
        '
        Me.cmdAvrStart.Location = New System.Drawing.Point(300, 25)
        Me.cmdAvrStart.Name = "cmdAvrStart"
        Me.cmdAvrStart.Size = New System.Drawing.Size(136, 22)
        Me.cmdAvrStart.TabIndex = 11
        Me.cmdAvrStart.Text = "Start Application"
        Me.cmdAvrStart.UseVisualStyleBackColor = True
        '
        'cmdAvrLoad
        '
        Me.cmdAvrLoad.Location = New System.Drawing.Point(11, 25)
        Me.cmdAvrLoad.Name = "cmdAvrLoad"
        Me.cmdAvrLoad.Size = New System.Drawing.Size(113, 22)
        Me.cmdAvrLoad.TabIndex = 10
        Me.cmdAvrLoad.Text = "Load File"
        Me.cmdAvrLoad.UseVisualStyleBackColor = True
        '
        'DfuPbBar
        '
        Me.DfuPbBar.Anchor = CType(((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.DfuPbBar.Location = New System.Drawing.Point(8, 269)
        Me.DfuPbBar.Name = "DfuPbBar"
        Me.DfuPbBar.Size = New System.Drawing.Size(430, 12)
        Me.DfuPbBar.TabIndex = 8
        '
        'SpiTab
        '
        Me.SpiTab.Controls.Add(Me.cbSPI)
        Me.SpiTab.Controls.Add(Me.RadioUseSpiSettings)
        Me.SpiTab.Controls.Add(Me.RadioUseSpiAuto)
        Me.SpiTab.Location = New System.Drawing.Point(4, 22)
        Me.SpiTab.Name = "SpiTab"
        Me.SpiTab.Padding = New System.Windows.Forms.Padding(3)
        Me.SpiTab.Size = New System.Drawing.Size(444, 289)
        Me.SpiTab.TabIndex = 3
        Me.SpiTab.Text = "SPI Settings"
        Me.SpiTab.UseVisualStyleBackColor = True
        '
        'cbSPI
        '
        Me.cbSPI.Controls.Add(Me.cbSpiProgMode)
        Me.cbSPI.Controls.Add(Me.cbUseEnWS)
        Me.cbSPI.Controls.Add(Me.txtEnWS)
        Me.cbSPI.Controls.Add(Me.lblSpiEnWrStatus)
        Me.cbSPI.Controls.Add(Me.lblSpiProgMode)
        Me.cbSPI.Controls.Add(Me.lblSpiInfo)
        Me.cbSPI.Controls.Add(Me.lblSpiClockDiv)
        Me.cbSPI.Controls.Add(Me.lblSpiMode)
        Me.cbSPI.Controls.Add(Me.lblSpiBitOrder)
        Me.cbSPI.Controls.Add(Me.txtChipSize)
        Me.cbSPI.Controls.Add(Me.lblSpiChipSize)
        Me.cbSPI.Controls.Add(Me.cbBitOrder)
        Me.cbSPI.Controls.Add(Me.cbSpiClock)
        Me.cbSPI.Controls.Add(Me.cbSpiMode)
        Me.cbSPI.Controls.Add(Me.txtChipErase)
        Me.cbSPI.Controls.Add(Me.lblSpiRead)
        Me.cbSPI.Controls.Add(Me.txtRead)
        Me.cbSPI.Controls.Add(Me.txtWriteStatus)
        Me.cbSPI.Controls.Add(Me.lblSpiSectorErase)
        Me.cbSPI.Controls.Add(Me.lblSpiWriteStatus)
        Me.cbSPI.Controls.Add(Me.txtSectorErase)
        Me.cbSPI.Controls.Add(Me.txtReadStatus)
        Me.cbSPI.Controls.Add(Me.lblSpiEraseSize)
        Me.cbSPI.Controls.Add(Me.lblSpiReadStatus)
        Me.cbSPI.Controls.Add(Me.txtEraseSize)
        Me.cbSPI.Controls.Add(Me.txtWriteEnable)
        Me.cbSPI.Controls.Add(Me.lblSpiWriteEn)
        Me.cbSPI.Controls.Add(Me.txtPageProgram)
        Me.cbSPI.Controls.Add(Me.lblSpiChipErase)
        Me.cbSPI.Controls.Add(Me.lblSpiPageProgram)
        Me.cbSPI.Location = New System.Drawing.Point(8, 30)
        Me.cbSPI.Name = "cbSPI"
        Me.cbSPI.Size = New System.Drawing.Size(428, 249)
        Me.cbSPI.TabIndex = 24
        Me.cbSPI.TabStop = False
        Me.cbSPI.Text = "SPI device commands"
        '
        'cbSpiProgMode
        '
        Me.cbSpiProgMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSpiProgMode.FormattingEnabled = True
        Me.cbSpiProgMode.Location = New System.Drawing.Point(304, 188)
        Me.cbSpiProgMode.Name = "cbSpiProgMode"
        Me.cbSpiProgMode.Size = New System.Drawing.Size(90, 21)
        Me.cbSpiProgMode.TabIndex = 26
        '
        'cbUseEnWS
        '
        Me.cbUseEnWS.AutoSize = True
        Me.cbUseEnWS.Location = New System.Drawing.Point(56, 192)
        Me.cbUseEnWS.Name = "cbUseEnWS"
        Me.cbUseEnWS.Size = New System.Drawing.Size(146, 17)
        Me.cbUseEnWS.TabIndex = 30
        Me.cbUseEnWS.Text = "Use 'Enable Write Status'"
        Me.cbUseEnWS.UseVisualStyleBackColor = True
        '
        'txtEnWS
        '
        Me.txtEnWS.Location = New System.Drawing.Point(8, 188)
        Me.txtEnWS.Name = "txtEnWS"
        Me.txtEnWS.Size = New System.Drawing.Size(36, 20)
        Me.txtEnWS.TabIndex = 29
        Me.txtEnWS.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'lblSpiEnWrStatus
        '
        Me.lblSpiEnWrStatus.AutoSize = True
        Me.lblSpiEnWrStatus.Location = New System.Drawing.Point(8, 172)
        Me.lblSpiEnWrStatus.Name = "lblSpiEnWrStatus"
        Me.lblSpiEnWrStatus.Size = New System.Drawing.Size(101, 13)
        Me.lblSpiEnWrStatus.TabIndex = 28
        Me.lblSpiEnWrStatus.Text = "Enable Write Status"
        '
        'lblSpiProgMode
        '
        Me.lblSpiProgMode.AutoSize = True
        Me.lblSpiProgMode.Location = New System.Drawing.Point(300, 168)
        Me.lblSpiProgMode.Name = "lblSpiProgMode"
        Me.lblSpiProgMode.Size = New System.Drawing.Size(76, 13)
        Me.lblSpiProgMode.TabIndex = 27
        Me.lblSpiProgMode.Text = "Program Mode"
        '
        'lblSpiInfo
        '
        Me.lblSpiInfo.AutoSize = True
        Me.lblSpiInfo.Location = New System.Drawing.Point(4, 224)
        Me.lblSpiInfo.Name = "lblSpiInfo"
        Me.lblSpiInfo.Size = New System.Drawing.Size(279, 13)
        Me.lblSpiInfo.TabIndex = 25
        Me.lblSpiInfo.Text = "Use the values commonly found in the device's datasheet"
        '
        'lblSpiClockDiv
        '
        Me.lblSpiClockDiv.AutoSize = True
        Me.lblSpiClockDiv.Location = New System.Drawing.Point(300, 72)
        Me.lblSpiClockDiv.Name = "lblSpiClockDiv"
        Me.lblSpiClockDiv.Size = New System.Drawing.Size(70, 13)
        Me.lblSpiClockDiv.TabIndex = 24
        Me.lblSpiClockDiv.Text = "Clock Divider"
        '
        'lblSpiMode
        '
        Me.lblSpiMode.AutoSize = True
        Me.lblSpiMode.Location = New System.Drawing.Point(300, 26)
        Me.lblSpiMode.Name = "lblSpiMode"
        Me.lblSpiMode.Size = New System.Drawing.Size(54, 13)
        Me.lblSpiMode.TabIndex = 23
        Me.lblSpiMode.Text = "SPI Mode"
        '
        'lblSpiBitOrder
        '
        Me.lblSpiBitOrder.AutoSize = True
        Me.lblSpiBitOrder.Location = New System.Drawing.Point(300, 120)
        Me.lblSpiBitOrder.Name = "lblSpiBitOrder"
        Me.lblSpiBitOrder.Size = New System.Drawing.Size(48, 13)
        Me.lblSpiBitOrder.TabIndex = 22
        Me.lblSpiBitOrder.Text = "Bit Order"
        '
        'txtChipSize
        '
        Me.txtChipSize.Location = New System.Drawing.Point(8, 44)
        Me.txtChipSize.Name = "txtChipSize"
        Me.txtChipSize.Size = New System.Drawing.Size(66, 20)
        Me.txtChipSize.TabIndex = 8
        Me.txtChipSize.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'lblSpiChipSize
        '
        Me.lblSpiChipSize.AutoSize = True
        Me.lblSpiChipSize.Location = New System.Drawing.Point(8, 26)
        Me.lblSpiChipSize.Name = "lblSpiChipSize"
        Me.lblSpiChipSize.Size = New System.Drawing.Size(51, 13)
        Me.lblSpiChipSize.TabIndex = 7
        Me.lblSpiChipSize.Text = "Chip Size"
        '
        'cbBitOrder
        '
        Me.cbBitOrder.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbBitOrder.FormattingEnabled = True
        Me.cbBitOrder.Location = New System.Drawing.Point(304, 140)
        Me.cbBitOrder.Name = "cbBitOrder"
        Me.cbBitOrder.Size = New System.Drawing.Size(90, 21)
        Me.cbBitOrder.TabIndex = 21
        '
        'cbSpiClock
        '
        Me.cbSpiClock.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSpiClock.FormattingEnabled = True
        Me.cbSpiClock.Location = New System.Drawing.Point(304, 92)
        Me.cbSpiClock.Name = "cbSpiClock"
        Me.cbSpiClock.Size = New System.Drawing.Size(90, 21)
        Me.cbSpiClock.TabIndex = 20
        '
        'cbSpiMode
        '
        Me.cbSpiMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSpiMode.FormattingEnabled = True
        Me.cbSpiMode.Location = New System.Drawing.Point(304, 44)
        Me.cbSpiMode.Name = "cbSpiMode"
        Me.cbSpiMode.Size = New System.Drawing.Size(90, 21)
        Me.cbSpiMode.TabIndex = 19
        '
        'txtChipErase
        '
        Me.txtChipErase.Location = New System.Drawing.Point(216, 140)
        Me.txtChipErase.Name = "txtChipErase"
        Me.txtChipErase.Size = New System.Drawing.Size(44, 20)
        Me.txtChipErase.TabIndex = 10
        Me.txtChipErase.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'lblSpiRead
        '
        Me.lblSpiRead.AutoSize = True
        Me.lblSpiRead.Location = New System.Drawing.Point(224, 26)
        Me.lblSpiRead.Name = "lblSpiRead"
        Me.lblSpiRead.Size = New System.Drawing.Size(33, 13)
        Me.lblSpiRead.TabIndex = 1
        Me.lblSpiRead.Text = "Read"
        '
        'txtRead
        '
        Me.txtRead.Location = New System.Drawing.Point(216, 44)
        Me.txtRead.Name = "txtRead"
        Me.txtRead.Size = New System.Drawing.Size(44, 20)
        Me.txtRead.TabIndex = 2
        Me.txtRead.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'txtWriteStatus
        '
        Me.txtWriteStatus.Location = New System.Drawing.Point(8, 140)
        Me.txtWriteStatus.Name = "txtWriteStatus"
        Me.txtWriteStatus.Size = New System.Drawing.Size(44, 20)
        Me.txtWriteStatus.TabIndex = 18
        Me.txtWriteStatus.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'lblSpiSectorErase
        '
        Me.lblSpiSectorErase.AutoSize = True
        Me.lblSpiSectorErase.Location = New System.Drawing.Point(212, 72)
        Me.lblSpiSectorErase.Name = "lblSpiSectorErase"
        Me.lblSpiSectorErase.Size = New System.Drawing.Size(68, 13)
        Me.lblSpiSectorErase.TabIndex = 3
        Me.lblSpiSectorErase.Text = "Sector Erase"
        '
        'lblSpiWriteStatus
        '
        Me.lblSpiWriteStatus.AutoSize = True
        Me.lblSpiWriteStatus.Location = New System.Drawing.Point(8, 120)
        Me.lblSpiWriteStatus.Name = "lblSpiWriteStatus"
        Me.lblSpiWriteStatus.Size = New System.Drawing.Size(65, 13)
        Me.lblSpiWriteStatus.TabIndex = 17
        Me.lblSpiWriteStatus.Text = "Write Status"
        '
        'txtSectorErase
        '
        Me.txtSectorErase.Location = New System.Drawing.Point(216, 92)
        Me.txtSectorErase.Name = "txtSectorErase"
        Me.txtSectorErase.Size = New System.Drawing.Size(44, 20)
        Me.txtSectorErase.TabIndex = 4
        Me.txtSectorErase.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'txtReadStatus
        '
        Me.txtReadStatus.Location = New System.Drawing.Point(112, 140)
        Me.txtReadStatus.Name = "txtReadStatus"
        Me.txtReadStatus.Size = New System.Drawing.Size(48, 20)
        Me.txtReadStatus.TabIndex = 16
        Me.txtReadStatus.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'lblSpiEraseSize
        '
        Me.lblSpiEraseSize.AutoSize = True
        Me.lblSpiEraseSize.Location = New System.Drawing.Point(8, 72)
        Me.lblSpiEraseSize.Name = "lblSpiEraseSize"
        Me.lblSpiEraseSize.Size = New System.Drawing.Size(57, 13)
        Me.lblSpiEraseSize.TabIndex = 5
        Me.lblSpiEraseSize.Text = "Erase Size"
        '
        'lblSpiReadStatus
        '
        Me.lblSpiReadStatus.AutoSize = True
        Me.lblSpiReadStatus.Location = New System.Drawing.Point(100, 120)
        Me.lblSpiReadStatus.Name = "lblSpiReadStatus"
        Me.lblSpiReadStatus.Size = New System.Drawing.Size(66, 13)
        Me.lblSpiReadStatus.TabIndex = 15
        Me.lblSpiReadStatus.Text = "Read Status"
        '
        'txtEraseSize
        '
        Me.txtEraseSize.Location = New System.Drawing.Point(8, 92)
        Me.txtEraseSize.Name = "txtEraseSize"
        Me.txtEraseSize.Size = New System.Drawing.Size(66, 20)
        Me.txtEraseSize.TabIndex = 6
        Me.txtEraseSize.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'txtWriteEnable
        '
        Me.txtWriteEnable.Location = New System.Drawing.Point(112, 92)
        Me.txtWriteEnable.Name = "txtWriteEnable"
        Me.txtWriteEnable.Size = New System.Drawing.Size(48, 20)
        Me.txtWriteEnable.TabIndex = 14
        Me.txtWriteEnable.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'lblSpiWriteEn
        '
        Me.lblSpiWriteEn.AutoSize = True
        Me.lblSpiWriteEn.Location = New System.Drawing.Point(100, 72)
        Me.lblSpiWriteEn.Name = "lblSpiWriteEn"
        Me.lblSpiWriteEn.Size = New System.Drawing.Size(68, 13)
        Me.lblSpiWriteEn.TabIndex = 13
        Me.lblSpiWriteEn.Text = "Write Enable"
        '
        'txtPageProgram
        '
        Me.txtPageProgram.Location = New System.Drawing.Point(112, 44)
        Me.txtPageProgram.Name = "txtPageProgram"
        Me.txtPageProgram.Size = New System.Drawing.Size(48, 20)
        Me.txtPageProgram.TabIndex = 12
        Me.txtPageProgram.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'lblSpiChipErase
        '
        Me.lblSpiChipErase.AutoSize = True
        Me.lblSpiChipErase.Location = New System.Drawing.Point(216, 120)
        Me.lblSpiChipErase.Name = "lblSpiChipErase"
        Me.lblSpiChipErase.Size = New System.Drawing.Size(58, 13)
        Me.lblSpiChipErase.TabIndex = 9
        Me.lblSpiChipErase.Text = "Chip Erase"
        '
        'lblSpiPageProgram
        '
        Me.lblSpiPageProgram.AutoSize = True
        Me.lblSpiPageProgram.Location = New System.Drawing.Point(100, 26)
        Me.lblSpiPageProgram.Name = "lblSpiPageProgram"
        Me.lblSpiPageProgram.Size = New System.Drawing.Size(74, 13)
        Me.lblSpiPageProgram.TabIndex = 11
        Me.lblSpiPageProgram.Text = "Page Program"
        '
        'RadioUseSpiSettings
        '
        Me.RadioUseSpiSettings.AutoSize = True
        Me.RadioUseSpiSettings.Location = New System.Drawing.Point(260, 8)
        Me.RadioUseSpiSettings.Name = "RadioUseSpiSettings"
        Me.RadioUseSpiSettings.Size = New System.Drawing.Size(112, 17)
        Me.RadioUseSpiSettings.TabIndex = 23
        Me.RadioUseSpiSettings.Text = "Use these settings"
        Me.RadioUseSpiSettings.UseVisualStyleBackColor = True
        '
        'RadioUseSpiAuto
        '
        Me.RadioUseSpiAuto.AutoSize = True
        Me.RadioUseSpiAuto.Checked = True
        Me.RadioUseSpiAuto.Location = New System.Drawing.Point(48, 8)
        Me.RadioUseSpiAuto.Name = "RadioUseSpiAuto"
        Me.RadioUseSpiAuto.Size = New System.Drawing.Size(132, 17)
        Me.RadioUseSpiAuto.TabIndex = 22
        Me.RadioUseSpiAuto.TabStop = True
        Me.RadioUseSpiAuto.Text = "Use automatic settings"
        Me.RadioUseSpiAuto.UseVisualStyleBackColor = True
        '
        'StatusStrip1
        '
        Me.StatusStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.Status})
        Me.StatusStrip1.Location = New System.Drawing.Point(0, 341)
        Me.StatusStrip1.Name = "StatusStrip1"
        Me.StatusStrip1.Size = New System.Drawing.Size(452, 22)
        Me.StatusStrip1.TabIndex = 3
        Me.StatusStrip1.Text = "StatusStrip1"
        '
        'Status
        '
        Me.Status.Name = "Status"
        Me.Status.Size = New System.Drawing.Size(129, 17)
        Me.Status.Text = "Welcome to FlashcatUSB!"
        '
        'MainForm
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(452, 363)
        Me.Controls.Add(Me.StatusStrip1)
        Me.Controls.Add(Me.MyTabs)
        Me.Controls.Add(Me.MenuStrip1)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.MainMenuStrip = Me.MenuStrip1
        Me.Name = "MainForm"
        Me.Text = "FlashcatUSB"
        Me.MenuStrip1.ResumeLayout(False)
        Me.MenuStrip1.PerformLayout()
        Me.MyTabs.ResumeLayout(False)
        Me.TabStatus.ResumeLayout(False)
        Me.TableLayoutPanel2.ResumeLayout(False)
        Me.TableLayoutPanel2.PerformLayout()
        Me.TableLayoutPanel1.ResumeLayout(False)
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabConsole.ResumeLayout(False)
        Me.TabConsole.PerformLayout()
        Me.AvrTab.ResumeLayout(False)
        Me.AvrTab.PerformLayout()
        Me.SpiTab.ResumeLayout(False)
        Me.SpiTab.PerformLayout()
        Me.cbSPI.ResumeLayout(False)
        Me.cbSPI.PerformLayout()
        Me.StatusStrip1.ResumeLayout(False)
        Me.StatusStrip1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents ContextMenuStrip1 As System.Windows.Forms.ContextMenuStrip
    Friend WithEvents MenuStrip1 As System.Windows.Forms.MenuStrip
    Friend WithEvents MainToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents MyTabs As System.Windows.Forms.TabControl
    Friend WithEvents TabStatus As System.Windows.Forms.TabPage
    Friend WithEvents TabConsole As System.Windows.Forms.TabPage
    Friend WithEvents StatusStrip1 As System.Windows.Forms.StatusStrip
    Friend WithEvents Status As System.Windows.Forms.ToolStripStatusLabel
    Friend WithEvents SettingsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents miDetectDevice As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents VerifyMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ScriptToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CurrentScript_MI As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents LoadScript_MI As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents UnloadScript_MI As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripSeparator1 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents ExitToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents PictureBox1 As System.Windows.Forms.PictureBox
    Friend WithEvents TableLayoutPanel1 As System.Windows.Forms.TableLayoutPanel
    Friend WithEvents lblStatus As System.Windows.Forms.Label
    Friend WithEvents TableLayoutPanel2 As System.Windows.Forms.TableLayoutPanel
    Friend WithEvents sm7 As System.Windows.Forms.Label
    Friend WithEvents sm6 As System.Windows.Forms.Label
    Friend WithEvents sm5 As System.Windows.Forms.Label
    Friend WithEvents sm4 As System.Windows.Forms.Label
    Friend WithEvents sm3 As System.Windows.Forms.Label
    Friend WithEvents sm2 As System.Windows.Forms.Label
    Friend WithEvents sm1 As System.Windows.Forms.Label
    Friend WithEvents AvrTab As System.Windows.Forms.TabPage
    Friend WithEvents DfuPbBar As System.Windows.Forms.ProgressBar
    Friend WithEvents SpiTab As System.Windows.Forms.TabPage
    Friend WithEvents ConsoleBox As System.Windows.Forms.ListBox
    Friend WithEvents cmdSaveLog As System.Windows.Forms.Button
    Friend WithEvents txtInput As System.Windows.Forms.TextBox
    Friend WithEvents lblAvrCrc As System.Windows.Forms.Label
    Friend WithEvents lblAvrRange As System.Windows.Forms.Label
    Friend WithEvents lblAvrFn As System.Windows.Forms.Label
    Friend WithEvents cmdAvrProg As System.Windows.Forms.Button
    Friend WithEvents cmdAvrStart As System.Windows.Forms.Button
    Friend WithEvents cmdAvrLoad As System.Windows.Forms.Button
    Friend WithEvents cbSPI As System.Windows.Forms.GroupBox
    Friend WithEvents cbSpiProgMode As System.Windows.Forms.ComboBox
    Friend WithEvents cbUseEnWS As System.Windows.Forms.CheckBox
    Friend WithEvents txtEnWS As System.Windows.Forms.TextBox
    Friend WithEvents lblSpiEnWrStatus As System.Windows.Forms.Label
    Friend WithEvents lblSpiProgMode As System.Windows.Forms.Label
    Friend WithEvents lblSpiInfo As System.Windows.Forms.Label
    Friend WithEvents lblSpiClockDiv As System.Windows.Forms.Label
    Friend WithEvents lblSpiMode As System.Windows.Forms.Label
    Friend WithEvents lblSpiBitOrder As System.Windows.Forms.Label
    Friend WithEvents txtChipSize As System.Windows.Forms.TextBox
    Friend WithEvents lblSpiChipSize As System.Windows.Forms.Label
    Friend WithEvents cbBitOrder As System.Windows.Forms.ComboBox
    Friend WithEvents cbSpiClock As System.Windows.Forms.ComboBox
    Friend WithEvents cbSpiMode As System.Windows.Forms.ComboBox
    Friend WithEvents txtChipErase As System.Windows.Forms.TextBox
    Friend WithEvents lblSpiRead As System.Windows.Forms.Label
    Friend WithEvents txtRead As System.Windows.Forms.TextBox
    Friend WithEvents txtWriteStatus As System.Windows.Forms.TextBox
    Friend WithEvents lblSpiSectorErase As System.Windows.Forms.Label
    Friend WithEvents lblSpiWriteStatus As System.Windows.Forms.Label
    Friend WithEvents txtSectorErase As System.Windows.Forms.TextBox
    Friend WithEvents txtReadStatus As System.Windows.Forms.TextBox
    Friend WithEvents lblSpiEraseSize As System.Windows.Forms.Label
    Friend WithEvents lblSpiReadStatus As System.Windows.Forms.Label
    Friend WithEvents txtEraseSize As System.Windows.Forms.TextBox
    Friend WithEvents txtWriteEnable As System.Windows.Forms.TextBox
    Friend WithEvents lblSpiWriteEn As System.Windows.Forms.Label
    Friend WithEvents txtPageProgram As System.Windows.Forms.TextBox
    Friend WithEvents lblSpiChipErase As System.Windows.Forms.Label
    Friend WithEvents lblSpiPageProgram As System.Windows.Forms.Label
    Friend WithEvents RadioUseSpiSettings As System.Windows.Forms.RadioButton
    Friend WithEvents RadioUseSpiAuto As System.Windows.Forms.RadioButton
    Friend WithEvents AvrEditor As FlashcatUSB.HexEditor
    Friend WithEvents tsiBootloader As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents LanguageToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents EnglishToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ChineseToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents FrenchToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents GermanToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents PortugueseToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents SpanishToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents EnableJTAGVccPinToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents SPINRF24LE1ModeToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem

End Class
