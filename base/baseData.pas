unit baseData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  SEC_SMALI = 'smali';
  SEC_XML = 'xml';
  SEC_HTML = 'html';
  SEC_CSS = 'css';
  SEC_JS = 'js';
  SEC_SHELL = 'shell';

  KEY_BACKGROUND = 'background';
  KEY_COMMENT_COLOR = 'comment_color';
  KEY_COMMENT_BOLD = 'comment_bold';
  KEY_IDENTIFIER_COLOR = 'identifier_color';
  KEY_IDENTIFIER_BOLD = 'identifier_bold';
  KEY_KEY_COLOR = 'key_color';
  KEY_KEY_BOLD = 'key_bold';
  KEY_SECOND_KEY_COLOR = 'second_key_color';
  KEY_SECOND_KEY_BOLD = 'second_key_bold';
  KEY_THIRD_KEY_COLOR = 'third_key_color';
  KEY_THIRD_KEY_BOLD = 'third_key_bold';
  KEY_NUMBER_COLOR = 'number_color';
  KEY_NUMBER_BOLD = 'number_bold';
  KEY_SPACE_COLOR = 'space_color';
  KEY_STRING_COLOR = 'string_color';
  KEY_STRING_BOLD = 'string_bold';
  KEY_SYMBOL_COLOR = 'symbol_color';
  KEY_SYMBOL_BOLD = 'symbol_bold';
  KEY_VAR_COLOR = 'var_color';
  KEY_VAR_BOLD = 'var_bold';
  KEY_ELEMENT_COLOR = 'element_color';
  KEY_ELEMENT_BOLD = 'element_bold';
  KEY_ATTR_COLOR = 'attr_color';
  KEY_ATTR_BOLD = 'attr_bold';
  KEY_NAMESPACE_COLOR = 'namespace_color';
  KEY_NAMESPACE_BOLD = 'namespace_bold';
  KEY_ATTRVALUE_COLOR = 'attrvalue_color';
  KEY_ATTRVALUE_BOLD = 'attrvalue_bold';
  KEY_NAMESPACEVALUE_COLOR = 'namespacevalue_color';
  KEY_NAMESPACEVALUE_BOLD = 'namespacevalue_bold';
  KEY_TEXT_COLOR = 'text_color';
  KEY_TEXT_BOLD = 'text_bold';
  KEY_CDATA_COLOR = 'cdata_color';
  KEY_CDATA_BOLD = 'cdata_bold';
  KEY_ENTITY_COLOR = 'entity_color';
  KEY_ENTITY_BOLD = 'entity_bold';
  KEY_PROCESSING_COLOR = 'processing_color';
  KEY_PROCESSING_BOLD = 'processing_bold';
  KEY_DOCTYPE_COLOR = 'doctype_color';
  KEY_DOCTYPE_BOLD = 'doctype_bold';
  KEY_AND_COLOR = 'and_color';
  KEY_AND_BOLD = 'and_bold';
  KEY_ASP_COLOR = 'asp_color';
  KEY_ASP_BOLD = 'asp_bold';
  KEY_UNDEF_KEY_COLOR = 'undef_key_color';
  KEY_UNDEF_KEY_BOLD = 'undef_key_bold';
  KEY_VALUE_COLOR = 'value_color';
  KEY_VALUE_BOLD = 'value_bold';
  KEY_MEASURE_COLOR = 'measure_color';
  KEY_MEASURE_BOLD = 'measure_bold';
  KEY_SELECTOR_COLOR = 'selector_color';
  KEY_SELECTOR_BOLD = 'selector_bold';
  KEY_NON_RESERVED_KEY_COLOR = 'non_reserved_key_color';
  KEY_NON_RESERVED_KEY_BOLD = 'non_reserved_key_bold';
  KEY_EVENT_COLOR = 'event_color';
  KEY_EVENT_BOLD = 'event_bold';

implementation

end.

