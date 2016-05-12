<html>
	<?php
//		print ("\$_POST[cgiusername] on an external page called with include is: " . $_POST[cgiusername]);
		print ("\n\$_POST[SID] on an external page called with include is: " . $_POST[SID]);
//		print ("\n\$_POST[cgipassword] on an external page called with include is: " . $_POST[cgipassword]);
	?>	
	<form name='LoginForm' method='post' action='/plb-bin/login.plc'>
		<input type='hidden' name='SID' value='<?php print $_POST[SID]; ?>' />               
	</form>
	<script language=javascript>

	</script>
</html>



