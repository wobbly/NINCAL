<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:template match="/">
	<html><head><title>Broker Report</title></head>
	<body>
	<IMG alt="Names in the News" src="logocolornotag.jpg" height="20%" />
	<table width="100%">
		<tr>
			<td width="25%">
				<table width="100%">
					<xsl:apply-templates select='//Broker' />
				</table>
			</td>
			<td width="25%">
			</td>
			<td width="25%">
			</td>
			<td width="25%">
				<table width="100%">
					<xsl:apply-templates select='//Date' />
				</table>
			</td>
		</tr>
	</table>

	<table width="100%">
		<tr>
			<td align='center'><i>Below is the status information on your order(s).  Please distribute to the listed Contact name(s).  Thank you.</i></td>
		</tr>
		<tr>
			<td align='center'><i>**All pricing subject to rate verification at time of order.  Please verify pricing at www.ninlists.com.</i></td>
		</tr>
	</table>
	<br />
	<xsl:apply-templates select='//Records' />
	</body>
	</html>
</xsl:template>

<xsl:template match="Broker">
	<tr>
		<td align="right">TO: </td>
		<td><xsl:value-of select="BrokerContact" /></td>
	</tr>
	<tr>
		<td align="right"></td>
		<td><xsl:value-of select="BrokerCompany" /></td>
	</tr>
</xsl:template>

<xsl:template match="Date">
	<tr>
		<td align="right">FROM: </td>
		<td>List Management</td>
	</tr>
	<tr>
		<td align="right">DATE: </td>
		<td><xsl:value-of select="." /></td>
	</tr>
</xsl:template>

<xsl:template match="Records">
	<table width="100%">
		<tr>
			<td align='center'>
			<span STYLE="font-family: 'arial'; color: '#000080'; font-size: '14'">
			<b>STATUS OF ORDERS RECEIVED</b>
			</span>
			</td>
		</tr>
	</table>
	<table width="100%">
		<tr>
			<td width="15%" valign='top'><b>Broker PO</b><br /><b>NIN #</b></td>
			<td width="30%" valign='top'><b>Contact</b><br /><b>Mailer</b><br /><b>List</b></td>
			<td width="15%" valign='top'><b>Quantity</b></td>
			<td width="15%" valign='top'><b>Request Date</b></td>
			<td width="20%" valign='top'><b>Status</b></td>
		</tr>
		<hr />
	</table>
	<xsl:choose>
		<xsl:when test='OrderRecord'>
			<xsl:apply-templates select='//OrderRecord' />
		</xsl:when>
		<xsl:otherwise>
			<table width="100%">
				<tr>
					<td align='center'>--NO PENDING ORDERS--</td>
				</tr>
			</table>
		</xsl:otherwise>
	</xsl:choose>

	<br />
	<table width="100%">
		<tr>
			<td align='center'>
			<span STYLE="font-family: 'arial'; color: '#000080'; font-size: '14'">
			<b>SHIPPING CONFIRMATION</b>
			</span>
			</td>
		</tr>
	</table>
	<table style="width:100%;">
		<tr>
			<td width="15%" valign='top'><b>Broker PO</b><br /><b>NIN #</b></td>
			<td width="30%" valign='top'><b>Contact</b><br /><b>Mailer</b><br /><b>List</b></td>
			<td width="15%" valign='top'><b>Quantity</b></td>
			<td width="15%" valign='top'><b>Ship Date</b></td>
			<td width="20%" valign='top'><b>Ship Method</b><br /><b>Tracker #</b></td>
		</tr>
		<hr />
	</table>
	<xsl:choose>
		<xsl:when test='ShippingRecord'>
			<xsl:apply-templates select='//ShippingRecord' />
		</xsl:when>
		<xsl:otherwise>
			<table width="100%">
				<tr>
					<td align='center'>--NO SHIPPING RECORDS--</td>
				</tr>
			</table>
		</xsl:otherwise>
	</xsl:choose>

	<br />
	<table style="width:100%;">
		<tr>
			<td align='center'>
			<span STYLE="font-family: 'arial'; color: '#000080'; font-size: '14'">
			<b>NEW LIST CLEARANCE REQUESTS</b>
			</span>
			</td>
		</tr>
	</table>
	<table style="width:100%;">
		<tr>
			<td width="15%" valign='top'><b>Broker PO</b><br /><b>NIN #</b></td>
			<td width="30%" valign='top'><b>Contact</b><br /><b>Mailer</b><br /><b>List</b></td>
			<td width="15%" valign='top'><b>Quantity</b></td>
			<td width="15%" valign='top'><b>Record Date</b></td>
			<td width="20%" valign='top'><b>Status</b></td>
		</tr>
		<hr />
	</table>
	<xsl:choose>
		<xsl:when test='NewLCRRecord'>
			<xsl:apply-templates select='//NewLCRRecord' />
		</xsl:when>
		<xsl:otherwise>
			<table width="100%">
				<tr>
					<td align='center'>--NO NEW LCRS--</td>
				</tr>
			</table>
		</xsl:otherwise>
	</xsl:choose>
</xsl:template>

<xsl:template match="OrderRecord">
	<table width="100%">
		<tr>
			<td width="15%" valign='top'>
				<span STYLE="font-family: 'arial'; font-size: '12'">
					<xsl:value-of select="LRRecord/PONumber" /><br /><xsl:value-of select="LRRecord/LRNumber" />
				</span>
			</td>
			<td width="30%" valign='top'>
				<span STYLE="font-family: 'arial'; font-size: '12'">
					<b><xsl:value-of select="//Broker/BrokerContact" /></b><br /><xsl:value-of select="Account/Mailer/MailerName" /><br /><xsl:value-of select="Account/List" />
				</span>
			</td>
			<td width="15%" valign='top'>
				<span STYLE="font-family: 'arial'; font-size: '12'">
					<xsl:value-of select="LRRecord/Quantity" />
				</span>
			</td>
			<td width="15%" valign='top'>
				<span STYLE="font-family: 'arial'; font-size: '12'">
					<xsl:value-of select="LRRecord/ReturnDate" />
				</span>
			</td>
			<td width="20%" valign='top'>
				<span STYLE="font-family: 'arial'; font-size: '12'">
					<xsl:value-of select="LRRecord/ClearStat" />
				</span>
			</td>
		</tr>
	</table>
</xsl:template>

<xsl:template match="ShippingRecord">
	<table width="100%">
		<tr>
			<td width="15%" valign='top'>
				<span STYLE="font-family: 'arial'; font-size: '12'">
					<xsl:value-of select="LRRecord/PONumber" /><br /><xsl:value-of select="LRRecord/LRNumber" />
				</span>
			</td>
			<td width="30%" valign='top'>
				<span STYLE="font-family: 'arial'; font-size: '12'">
					<b><xsl:value-of select="//Broker/BrokerContact" /></b><br /><xsl:value-of select="Account/Mailer/MailerName" /><br /><xsl:value-of select="Account/List" />
				</span>
			</td>
			<td width="15%" valign='top'>
				<span STYLE="font-family: 'arial'; font-size: '12'">
					<xsl:value-of select="Shipping/ShipQuantity" />
				</span>
			</td>
			<td width="15%" valign='top'>
				<span STYLE="font-family: 'arial'; font-size: '12'">
					<xsl:value-of select="Shipping/ShipDate" />
				</span>
			</td>
			<td width="20%" valign='top'>
				<span STYLE="font-family: 'arial'; font-size: '12'">
					<xsl:value-of select="Shipping/ShipText" /><br /><xsl:value-of select="Shipping/ShipTracking" />
				</span>
			</td>

		</tr>
	</table>
</xsl:template>

<xsl:template match="NewLCRRecord">
	<table width="100%">
		<tr>
			<td width="15%" valign='top'>
				<span STYLE="font-family: 'arial'; font-size: '12'">
					<xsl:value-of select="LRRecord/PONumber" /><br /><xsl:value-of select="LRRecord/LRNumber" />
				</span>
			</td>
			<td width="30%" valign='top'>
				<span STYLE="font-family: 'arial'; font-size: '12'">
					<b><xsl:value-of select="//Broker/BrokerContact" /></b><br /><xsl:value-of select="Account/Mailer/MailerName" /><br /><xsl:value-of select="Account/List" />
				</span>
			</td>
			<td width="15%" valign='top'>
				<span STYLE="font-family: 'arial'; font-size: '12'">
					<xsl:value-of select="LRRecord/Quantity" />
				</span>
			</td>
			<td width="15%" valign='top'>
				<span STYLE="font-family: 'arial'; font-size: '12'">
					<xsl:value-of select="LRRecord/OrderDate" />
				</span>
			</td>
			<td width="20%" valign='top'>
				<span STYLE="font-family: 'arial'; font-size: '12'">
					<xsl:value-of select="LRRecord/ClearStat" />
				</span>
			</td>
		</tr>
	</table>
</xsl:template>

</xsl:stylesheet>
