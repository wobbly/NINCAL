#=====================================================================================================#
#    Copyright 2011 Robert Stacks
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#=====================================================================================================#
# Author: Robert Stacks
# URL: RandomTechMinutia.wordpress.com
# Updated: 11/15/2011
# Verson: 1.0
#
# Based on a script by Dan Penning http://danpenning.com
#
# Purpose:
# Powershell script to find out a list of users
# whose password is expiring within x number of days (as specified in $days_before_expiry).
# Email notification will be sent to them reminding them that they need to change their password.
#
# Requirements:
# Quest ActiveRoles cmdlets (http://www.quest.com/powershell/activeroles-server.aspx)
# Powershell 2.0 ~ may work in 1.0 but I haven't tested it.
#
# Script must be run as a user with Permission to view AD Attributes, Domain Admin for example.
#
#=====================================================================================================#
#Add Snapins
Add-PSSnapin "Quest.ActiveRoles.ADManagement" -ErrorAction SilentlyContinue

#Get todays date for use in the script
$date = Get-Date

#===========================#
#User Adjustable Variables  #
#===========================#

# How many Days Advanced Warning do you want to give?
$DaysOfNotice = 14

#Generate a Admin report?
$ReportToAdmin = $true
#$ReportToAdmin = $false

#Sort Report
#===========================#
# 0 = By OU
# 1 = First Name Ascending
# 2 = Last Name Ascending
# 3 = Expiration Date Ascending
# 4 = First Name Descending
# 5 = Last Name Descending
# 6 = Expiration Date Descending
#===========================#
$ReportSortBy=1

#Alert User?
$AlertUser = $true
#$AlertUser = $false

#URL for self Service
$URLToSelfService = "http://"

#Mail Server Variables
$FromAddress = "ComputerRequest@NINCAL.COM"
$RelayMailServer = "ninmail.nincal.com"
$AdminEmailAddress ="DavidHerrick@NINCAL.COM"

# Define font and font size
# ` or \ is an escape character in powershell
$font = "<font size=`"3`" face=`"Calibri`">"

# List of users whose Password is about to expire. The following line can be added to limit OU's searched if so desired
#$users += Get-QADUser -SearchRoot 'DN' -Enabled -PasswordNeverExpires:$false | where {($_.PasswordExpires -lt $date.AddDays($DaysOfNotice))}
#$users = Get-QADUser -SearchRoot 'OU= Company USERS,DC=NINCAL,DC=com' -Enabled -PasswordNeverExpires:$false | where {($_.PasswordExpires -lt $date.AddDays($DaysOfNotice))}
#$users += Get-QADUser -SearchRoot 'OU=INTERNATIONAL OFFICE USERS,DC=NINCAL,DC=com' -Enabled -PasswordNeverExpires:$false | where {($_.PasswordExpires -lt $date.AddDays($DaysOfNotice))}

# Search Whole Root
$users = Get-QADUser -Enabled -PasswordNeverExpires:$false | where {($_.PasswordExpires -lt $date.AddDays($DaysOfNotice))}

#===========================#
#Main Script                #
#===========================#

# Sort Report
Switch ($ReportSortBy)
{
'0' {$users}
'1' {$users = $users | sort {$_.FirstName}}
'2' {$users = $users | sort {$_.LastName}}
'3' {$users = $users | sort {$_.PasswordExpires}}
'4' {$users = $users | sort -Descending {$_.FirstName}}
'5' {$users = $users | sort -Descending {$_.LastName}}
'6' {$users = $users | sort -Descending {$_.PasswordExpires}}
}

if ($ReportToAdmin -eq $true)
{
          #Headings used in the Admin Alert
          $Title="<h1><u>Password Expiration Status and Alert Report</h1></u><h4>Generated on " + $date + "</h4>"
          $Title+="<font color = red><h2><u>Admin Action Required</h2></u></font>"
          $Title+="<font size=`"3`" color = red> An Admin needs to update these accounts so that users can be notified of pending or past password experation<br></font>"
          $Title_ExpiredNoEmail="<h3><u>Users Have Expired Passwords And No Primary SMTP to Notify Them</h3></u>"
          $Title_AboutToExpireNoEmail="<h3><u>Users Password's Is About To Expire That Have No Primary SMTP Address</h3></u>"
          $Title2="<br><br><h2><u><font color= red>No Admin Action Required - Email Sent to User</h2></u></font>"
          $Title_Expired="<h3><u>Users With Expired Passwords</h3></u>"
          $Title_AboutToExpire="<h3><u>Users Password's About To Expire</h3></u>"
          $Title_NoExpireDate="<h3><u>Users with no Expiration Date</u></h3>"
}
#For loop to report
foreach ($user in $users)
{

           if ($user.PasswordExpires -eq $null)
           {
                    $UsersList_WithNoExpiration += $user.Name + " (<font color=blue>" + $user.LogonName + "</font>) does not seem to have a Expiration Date on their account.<font color=Green> <br>OU Container: " + $user.DN + "</font> <br>"
           }
           Elseif ($user.PasswordExpires -ne $null)
           {
                    #Calculate remaining days till Password Expires
                    $DaysLeft = (($user.PasswordExpires - $date).Days)

                    #Days till password expires
                    $DaysLeftTillExpire = [Math]::Abs($DaysLeft)

                              #If password has expired
                              If ($DaysLeft -le 0)
                              {
                                        #If the users don't have a primary SMTP address we'll report the problem in the Admin report
                                        if (($user.Email -eq $null) -and ($user.UserMustChangePassword -ne $true) -and ($ReportToAdmin -eq $true))
                                        {
                                        #Add it to admin list to report on it
                                        $UserList_ExpiredNoEmail += $user.name + " (<font color=blue>" + $user.LogonName + "</font>) password has expired " + $DaysLeftTillExpire + " day(s) ago</font>. <font color=Green> <br>OU Container: " + $user.DN + "</font> <br><br>"
                                        }

                                        #Else they have an email address and we'll add this to the admin report and email the user.
                                        elseif (($user.Email -ne $null) -and ($user.UserMustChangePassword -eq $true) -and ($AlertUser -eq $True))
                                        {
                                                  if ($ReportToAdmin -eq $true)
                                                  {
                                                            #Add it to a list
                                                            $UserList_ExpiredHasEmail += $user.name + " (<font color=blue>" + $user.LogonName + "</font>) password has expired " + $DaysLeftTillExpire + " day(s) ago</font>. <font color=Green> <br>OU Container: " + $user.DN + "</font> <br><br>"
                                                  }

                                                  $ToAddress = $user.Email
                                                  $Subject = "Friendly Reminder: Your Corporate Password has expired."
                                                  $body = " "
                                                  $body = $font
                                                  $body += "Greetings, <br><br>"
                                                  $body += "This is a auto-generated email to remind you (<font color=blue> " + $user.Name + "</font>) that your Corporate Password for account - <font color=red>" + $user.LogonName + "</font> - has expired. <br><br>"
                                                  $body += "This means you will not be able to access any secured system hosted at the Corporate Offices.<br><br> You can log into " + $URLToSelfService + " "
                                                  $body += "and reset the password yourself, else you are welcome to contact the Service Desk by Phone for assistance."
                                                  $body += "<br><br><br><br>"
                                                  $body += " "
                                                  $body += "<h4>Never Share Your Password With Others!</h4>"
                              $body += "<h5>Message generated on: " + $date + ".</h5>"
                              $body += "</font>"

                                                  Send-MailMessage -smtpServer $RelayMailServer -from $FromAddress -to $user.Email -subject $Subject -BodyAsHtml  -body $body

                                        }
                              }
                              elseif ($DaysLeft -ge 0)
                              {
                                        #If Password is about to expire but the user doesn't have a primary address report that in the Admin report
                                        if (($user.Email -eq $null) -and ($user.UserMustChangePassword -ne $true) -and ($ReportToAdmin -eq $true))
                                        {
                                        #Add it to admin list
                                        $UserList_AboutToExpireNoEmail += $user.name + " (<font color=blue>" + $user.LogonName + "</font>) password is about to expire and has " + $DaysLeftTillExpire + " day(s) left</font>. <font color=Green> <br>OU Container: " + $user.DN + "</font> <br><br>"
                                        }
                                        # If there is an email address assigned to the AD Account send them a email and also report it in the admin report
                                        elseif (($user.Email -ne $null) -and ($user.UserMustChangePassword -ne $true) -and ($AlertUser -eq $True) )
                                        {
                                                  if ($ReportToAdmin -eq $true)
                                                  {
                                                            #Add it to admin Report list
                                                  $UserList_AboutToExpire += $user.name + "  <font color=blue>(" + $user.LogonName + "</font>) password is about to expire and has " + $DaysLeftTillExpire + " day(s) left</font>. <font color=Green> <br>OU Container: " + $user.DN + "</font> <br><br>"
                                                  }

                                                  #Setup email to be sent to user
                                                  $ToAddress = $user.Email
                                                  $Subject = "Notice: Your Corporate Password is about to expire."
                                                  $body = " "
                                                  $body = $font
                                                  $body += "Greetings, <br><br>"
                                                  $body += "This is a auto-generated email to remind you (<font color=blue> " + $user.Name + "</font>) that your Corporate Password for account - <font color=red>" + $user.LogonName + "</font> - will expire in </font color = red>" + $DaysLeftTillExpire +" Day(s). <br><br>"
                                                  $body += "This means you will not be able to access any secured system hosted at the Corporate Offices.<br><br> You can log into " + $URLToSelfService + " "
                                                  $body += "and reset the password yourself, else you are welcome to contact the Service Desk by Phone for assistance."
                                                  $body += "<br><br><br><br>"
                                                  $body += "<h4>Never Share Your Password With Others!</h4>"
                              $body += "<h5>Auto-Generated Message On: " + $date + ".</h5>"
                              $body += "</font>"

                                                  Send-MailMessage -smtpServer $RelayMailServer -from $FromAddress -to $user.Email -subject $Subject -BodyAsHtml  -body $body

                                        }
                              }
          }
} # End foreach ($user in $users)

if ($ReportToAdmin -eq $true)
{
 If ($UserList_AboutToExpire -eq $null)  {$UserList_AboutToExpire = "No Users to Report"}
 If ($UserList_AboutToExpireNoEmail -eq $null){ $UserList_AboutToExpireNoEmail = "No Users to Report"}
 if ($UserList_ExpiredHasEmail -eq $null) {$UserList_ExpiredHasEmail = "No Users to Report"}
 if ($UserList_ExpiredNoEmail -eq $null) {$UserList_ExpiredNoEmail = "No Users to Report"}
 if ($UsersList_WithNoExpiration -eq $null) {$UsersList_WithNoExpiration = "No Users to Report"}

 #Email Report to Admin
 $Subject="Password Expiration Status for " + $date + "."
 $AdminReport = $font + $Title + $Title_ExpiredNoEmail + $UserList_ExpiredNoEmail + $Title_AboutToExpireNoEmail + $UserList_AboutToExpireNoEmail + $Title_AboutToExpire + $UserList_AboutToExpire + $Title_Expired + $UserList_ExpiredHasEmail + $Title_NoExpireDate + $UsersList_WithNoExpiration + "</font>"
 Send-MailMessage -smtpServer $RelayMailServer -from $FromAddress -to $AdminEmailAddress -subject $Subject -BodyAsHtml -body $AdminReport
}
