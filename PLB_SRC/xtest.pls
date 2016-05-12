NODE_ELEMENT                  CONST  "1" The node represents an element. An element node can have the following child node types: Element, Text, Comment, ProcessingInstruction, CDATASection, and EntityReference. An element node can be the child of the Document, DocumentFragment, EntityReference, and Element nodes.
NODE_ATTRIBUTE                CONST     "2"       The node represents an attribute of an element. An attribute node can have the following child node types: Text and EntityReference. The attribute does not appear as the child node of any other node type; note that it is not considered a child node of an element.
NODE_TEXT                     CONST     "3"       The node represents the text content of a tag. A text node cannot have any child nodes. The text node can appear as the child node of the Attribute, DocumentFragment, Element, and EntityReference nodes.
NODE_CDATA_SECTION            CONST     "4"       The node represents a CDATA section in the XML source. CDATA sections are used to escape blocks of text that would otherwise be recognized as markup. A CDATA section node cannot have any child nodes. The CDATA section node can appear as the child of the DocumentFragment, EntityReference, and Element nodes.
NODE_ENTITY_REFERENCE         CONST     "5"       The node represents a reference to an entity in the XML document. This applies to all entities, including character entity references. An entity reference node can have the following child node types: Element, ProcessingInstruction, Comment, Text, CDATASection, and EntityReference. The entity reference node can appear as the child of the Attribute, DocumentFragment, Element, and EntityReference nodes.
NODE_ENTITY                   CONST     "6"       The node represents an expanded entity. An entity node can have child nodes that represent the expanded entity (for example, Text and EntityReference nodes). The entity node can appear as the child of the DocumentType node.
NODE_PROCESSING_INSTRUCTION   CONST     "7"       The node represents a processing instruction (PI) from the XML document. A PI node cannot have any child nodes. The PI node can appear as the child of the Document, DocumentFragment, Element, and EntityReference nodes.
NODE_COMMENT                  CONST     "8"       The node represents a comment in the XML document. A comment node cannot have any child nodes. The comment node can appear as the child of Document, DocumentFragment, Element, and EntityReference nodes.
NODE_DOCUMENT                 CONST     "9"       The node represents a document object, which, as the root of the document tree, provides access to the entire XML document. It is created using the ProgID "Microsoft.XMLDOM", or through a data island using <XML> or <SCRIPT LANGUAGE=XML>. The document node can have the following child node types: Element (maximum of one), ProcessingInstruction, Comment, and DocumentType. The document node cannot appear as the child of any node types.
NODE_DOCUMENT_TYPE            CONST     "10"      The node represents the document type declaration, indicated by the <!DOCTYPE > tag. The document type node can have the following child node types: Notation and Entity. The document type node can appear as the child of the Document node.
NODE_DOCUMENT_FRAGMENT        CONST     "11"      The node represents a document fragment. The document fragment node associates a node or subtree with a document without actually being contained within the document. The document fragment node can have the following child node types: Element, ProcessingInstruction, Comment, Text, CDATASection, and EntityReference. The DocumentFragment node cannot appear as the child of any node types.
NODE_NOTATION                 CONST     "12"      A node represents a notation in the document type declaration. The notation node cannot have any child nodes. The notation node can appear as the child of the DocumentType node.




IXMLDOC   AUTOMATION          CLASS="Microsoft.XMLDOM"
nodelist  AUTOMATION
nodelist2 AUTOMATION
nodelist3 AUTOMATION
root      AUTOMATION
xNode     AUTOMATION
xNode2     AUTOMATION
result    form      1
f1        form      1
text_d    dim       100
count     form      6
count2     form      6
count3    form      6
x         form      6
ntype     form      6
tstring   dim       100
indx      form      6
indx2     form      6
          winshow
          create    ixmldoc
          ixmldoc.load giving result using "w:\bas.xml"
          if (result)
             alert caution,"opened ok",f1
             call   do_something
          else
             alert caution,"open failed",f1
          endif
          DESTROY   Ixmldoc
          keyin f1
          stop
do_something
          GETPROP   ixmldoc,*documentelement=root
          GETPROP   root,*childnodes=nodelist
          GETPROP   nodelist,*length=count
          for indx of "0" to (count-1)
             nodelist.item giving xnode using indx
             call      get_values
          repeat
          return
get_values
          getprop   xnode,*childnodes=nodelist2
          getprop   nodelist2,*length=count2
          for indx2 of "0" to (count2-1)
             nodelist2.item giving xnode2 using indx2
             getprop   xnode2,*text=text_d
             display   text_d
          repeat
          return
