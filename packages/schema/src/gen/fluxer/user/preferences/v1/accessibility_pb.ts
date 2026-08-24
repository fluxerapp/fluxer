import type { GenEnum, GenFile, GenMessage } from "@bufbuild/protobuf/codegenv2";
import { enumDesc, fileDesc, messageDesc } from "@bufbuild/protobuf/codegenv2";
import type { Message } from "@bufbuild/protobuf";

/**
 * Describes the file fluxer/user/preferences/v1/accessibility.proto.
 */
export const file_fluxer_user_preferences_v1_accessibility: GenFile = /*@__PURE__*/
  fileDesc("Ci5mbHV4ZXIvdXNlci9wcmVmZXJlbmNlcy92MS9hY2Nlc3NpYmlsaXR5LnByb3RvEhpmbHV4ZXIudXNlci5wcmVmZXJlbmNlcy52MSKHIgoVQWNjZXNzaWJpbGl0eVNldHRpbmdzEh4KEXNhdHVyYXRpb25fZmFjdG9yGAEgASgBSACIAQESHgoWYWx3YXlzX3VuZGVybGluZV9saW5rcxgCIAEoCBIiChVlbmFibGVfdGV4dF9zZWxlY3Rpb24YAyABKAhIAYgBARIlChhzaG93X21lc3NhZ2Vfc2VuZF9idXR0b24YBCABKAhIAogBARIlChhzaG93X3RleHRhcmVhX2ZvY3VzX3JpbmcYBSABKAhIA4gBARIbChNoaWRlX2tleWJvYXJkX2hpbnRzGAYgASgIEicKGmVzY2FwZV9leGl0c19rZXlib2FyZF9tb2RlGAcgASgISASIAQESLAofc3luY19yZWR1Y2VkX21vdGlvbl93aXRoX3N5c3RlbRgIIAEoCEgFiAEBEiQKF3JlZHVjZWRfbW90aW9uX292ZXJyaWRlGAkgASgISAaIAQESIgoVbWVzc2FnZV9ncm91cF9zcGFjaW5nGAogASgBSAeIAQESGwoObWVzc2FnZV9ndXR0ZXIYCyABKAFICIgBARIWCglmb250X3NpemUYDCABKAFICYgBARIuCiFzaG93X3VzZXJfYXZhdGFyc19pbl9jb21wYWN0X21vZGUYDSABKAhICogBARIrCiNtb2JpbGVfc3RpY2tlcl9hbmltYXRpb25fb3ZlcnJpZGRlbhgOIAEoCBImCh5tb2JpbGVfZ2lmX2F1dG9wbGF5X292ZXJyaWRkZW4YDyABKAgSJwofbW9iaWxlX2FuaW1hdGVfZW1vamlfb3ZlcnJpZGRlbhgQIAEoCBIrCh5tb2JpbGVfc3RpY2tlcl9hbmltYXRpb25fdmFsdWUYESABKAVIC4gBARImChltb2JpbGVfZ2lmX2F1dG9wbGF5X3ZhbHVlGBIgASgISAyIAQESJwoabW9iaWxlX2FuaW1hdGVfZW1vamlfdmFsdWUYEyABKAhIDYgBARIcChRhdXRvX3NlbmRfa2xpcHlfZ2lmcxgUIAEoCBIcCg9zaG93X2dpZl9idXR0b24YFSABKAhIDogBARIeChFzaG93X21lbWVzX2J1dHRvbhgWIAEoCEgPiAEBEiEKFHNob3dfc3RpY2tlcnNfYnV0dG9uGBcgASgISBCIAQESHgoRc2hvd19lbW9qaV9idXR0b24YGCABKAhIEYgBARInChpzaG93X21lZGlhX2Zhdm9yaXRlX2J1dHRvbhgZIAEoCEgSiAEBEicKGnNob3dfbWVkaWFfZG93bmxvYWRfYnV0dG9uGBogASgISBOIAQESJQoYc2hvd19tZWRpYV9kZWxldGVfYnV0dG9uGBsgASgISBSIAQESKAobc2hvd19zdXBwcmVzc19lbWJlZHNfYnV0dG9uGBwgASgISBWIAQESHwoSc2hvd19naWZfaW5kaWNhdG9yGB0gASgISBaIAQESLQogc2hvd19hdHRhY2htZW50X2V4cGlyeV9pbmRpY2F0b3IYHiABKAhIF4gBARIvCiJ1c2VfYnJvd3Nlcl9sb2NhbGVfZm9yX3RpbWVfZm9ybWF0GB8gASgISBiIAQESXQodY2hhbm5lbF90eXBpbmdfaW5kaWNhdG9yX21vZGUYICABKA4yNi5mbHV4ZXIudXNlci5wcmVmZXJlbmNlcy52MS5DaGFubmVsVHlwaW5nSW5kaWNhdG9yTW9kZRIzCiZzaG93X3NlbGVjdGVkX2NoYW5uZWxfdHlwaW5nX2luZGljYXRvchghIAEoCEgZiAEBEiQKF3Nob3dfbWVzc2FnZV9hY3Rpb25fYmFyGCIgASgISBqIAQESNAonc2hvd19tZXNzYWdlX2FjdGlvbl9iYXJfcXVpY2tfcmVhY3Rpb25zGCMgASgISBuIAQESMQokc2hvd19tZXNzYWdlX2FjdGlvbl9iYXJfc2hpZnRfZXhwYW5kGCQgASgISByIAQESNQooc2hvd19tZXNzYWdlX2FjdGlvbl9iYXJfb25seV9tb3JlX2J1dHRvbhglIAEoCEgdiAEBEjAKI3Nob3dfZGVmYXVsdF9lbW9qaXNfaW5fYXV0b2NvbXBsZXRlGCYgASgISB6IAQESLwoic2hvd19jdXN0b21fZW1vamlzX2luX2F1dG9jb21wbGV0ZRgnIAEoCEgfiAEBEioKHXNob3dfc3RpY2tlcnNfaW5fYXV0b2NvbXBsZXRlGCggASgISCCIAQESJwoac2hvd19tZW1lc19pbl9hdXRvY29tcGxldGUYKSABKAhIIYgBARI1Cih2b2ljZV9jaGFubmVsX2pvaW5fcmVxdWlyZXNfZG91YmxlX2NsaWNrGCwgASgISCKIAQESHQoQY3VzdG9tX3RoZW1lX2NzcxgtIAEoCUgjiAEBEhsKDnNob3dfZmF2b3JpdGVzGC4gASgISCSIAQESFwoKem9vbV9sZXZlbBgvIAEoAUgliAEBElEKF2RtX21lc3NhZ2VfcHJldmlld19tb2RlGDAgASgOMjAuZmx1eGVyLnVzZXIucHJlZmVyZW5jZXMudjEuRG1NZXNzYWdlUHJldmlld01vZGUSHwoSZW5hYmxlX3R0c19jb21tYW5kGDEgASgISCaIAQESFQoIdHRzX3JhdGUYMiABKAFIJ4gBARIwCiNzaG93X2ZhZGVkX3VucmVhZF9vbl9tdXRlZF9jaGFubmVscxgzIAEoCEgoiAEBEigKG3Nob3dfY29udGV4dF9tZW51X3Nob3J0Y3V0cxg0IAEoCEgpiAEBEioKHWNvbmZpcm1fYmVmb3JlX3N0YXJ0aW5nX2NhbGxzGDUgASgISCqIAQESRAoQaGRyX2Rpc3BsYXlfbW9kZRg2IAEoDjIqLmZsdXhlci51c2VyLnByZWZlcmVuY2VzLnYxLkhkckRpc3BsYXlNb2RlEiAKE3ByZXNlcnZlX2VkaXRfZHJhZnQYNyABKAhIK4gBARIsCh9zdGF5X2ludGVyYWN0aXZlX3doZW5fdW5mb2N1c2VkGDggASgISCyIAQESMgolY29uZmlybV9iZWZvcmVfam9pbmluZ192b2ljZV9jaGFubmVscxg5IAEoCEgtiAEBEjAKI3NjcmVlbl9yZWFkZXJfYW5ub3VuY2VfbmV3X21lc3NhZ2VzGDogASgISC6IAQESNAonZmlyc3RfY2xpY2tfcGFzc190aHJvdWdoX3doZW5fdW5mb2N1c2VkGDsgASgISC+IAQESKgodY29tcGFjdF9tZXNzYWdlX2dyb3VwX3NwYWNpbmcYPCABKAFIMIgBARItCiBzY3JvbGxfdG9fYm90dG9tX29uX21lc3NhZ2Vfc2VuZBg9IAEoCEgxiAEBEiMKFmRpbV9zdHJpa2V0aHJvdWdoX3RleHQYPiABKAhIMogBARIhChRzZXF1ZW50aWFsX2ZpbGVfc2VuZBg/IAEoCEgziAEBEikKHG1vYmlsZV9zcGxhc2hfem9vbV9hbmltYXRpb24YQCABKAhINIgBAUIUChJfc2F0dXJhdGlvbl9mYWN0b3JCGAoWX2VuYWJsZV90ZXh0X3NlbGVjdGlvbkIbChlfc2hvd19tZXNzYWdlX3NlbmRfYnV0dG9uQhsKGV9zaG93X3RleHRhcmVhX2ZvY3VzX3JpbmdCHQobX2VzY2FwZV9leGl0c19rZXlib2FyZF9tb2RlQiIKIF9zeW5jX3JlZHVjZWRfbW90aW9uX3dpdGhfc3lzdGVtQhoKGF9yZWR1Y2VkX21vdGlvbl9vdmVycmlkZUIYChZfbWVzc2FnZV9ncm91cF9zcGFjaW5nQhEKD19tZXNzYWdlX2d1dHRlckIMCgpfZm9udF9zaXplQiQKIl9zaG93X3VzZXJfYXZhdGFyc19pbl9jb21wYWN0X21vZGVCIQofX21vYmlsZV9zdGlja2VyX2FuaW1hdGlvbl92YWx1ZUIcChpfbW9iaWxlX2dpZl9hdXRvcGxheV92YWx1ZUIdChtfbW9iaWxlX2FuaW1hdGVfZW1vamlfdmFsdWVCEgoQX3Nob3dfZ2lmX2J1dHRvbkIUChJfc2hvd19tZW1lc19idXR0b25CFwoVX3Nob3dfc3RpY2tlcnNfYnV0dG9uQhQKEl9zaG93X2Vtb2ppX2J1dHRvbkIdChtfc2hvd19tZWRpYV9mYXZvcml0ZV9idXR0b25CHQobX3Nob3dfbWVkaWFfZG93bmxvYWRfYnV0dG9uQhsKGV9zaG93X21lZGlhX2RlbGV0ZV9idXR0b25CHgocX3Nob3dfc3VwcHJlc3NfZW1iZWRzX2J1dHRvbkIVChNfc2hvd19naWZfaW5kaWNhdG9yQiMKIV9zaG93X2F0dGFjaG1lbnRfZXhwaXJ5X2luZGljYXRvckIlCiNfdXNlX2Jyb3dzZXJfbG9jYWxlX2Zvcl90aW1lX2Zvcm1hdEIpCidfc2hvd19zZWxlY3RlZF9jaGFubmVsX3R5cGluZ19pbmRpY2F0b3JCGgoYX3Nob3dfbWVzc2FnZV9hY3Rpb25fYmFyQioKKF9zaG93X21lc3NhZ2VfYWN0aW9uX2Jhcl9xdWlja19yZWFjdGlvbnNCJwolX3Nob3dfbWVzc2FnZV9hY3Rpb25fYmFyX3NoaWZ0X2V4cGFuZEIrCilfc2hvd19tZXNzYWdlX2FjdGlvbl9iYXJfb25seV9tb3JlX2J1dHRvbkImCiRfc2hvd19kZWZhdWx0X2Vtb2ppc19pbl9hdXRvY29tcGxldGVCJQojX3Nob3dfY3VzdG9tX2Vtb2ppc19pbl9hdXRvY29tcGxldGVCIAoeX3Nob3dfc3RpY2tlcnNfaW5fYXV0b2NvbXBsZXRlQh0KG19zaG93X21lbWVzX2luX2F1dG9jb21wbGV0ZUIrCilfdm9pY2VfY2hhbm5lbF9qb2luX3JlcXVpcmVzX2RvdWJsZV9jbGlja0ITChFfY3VzdG9tX3RoZW1lX2Nzc0IRCg9fc2hvd19mYXZvcml0ZXNCDQoLX3pvb21fbGV2ZWxCFQoTX2VuYWJsZV90dHNfY29tbWFuZEILCglfdHRzX3JhdGVCJgokX3Nob3dfZmFkZWRfdW5yZWFkX29uX211dGVkX2NoYW5uZWxzQh4KHF9zaG93X2NvbnRleHRfbWVudV9zaG9ydGN1dHNCIAoeX2NvbmZpcm1fYmVmb3JlX3N0YXJ0aW5nX2NhbGxzQhYKFF9wcmVzZXJ2ZV9lZGl0X2RyYWZ0QiIKIF9zdGF5X2ludGVyYWN0aXZlX3doZW5fdW5mb2N1c2VkQigKJl9jb25maXJtX2JlZm9yZV9qb2luaW5nX3ZvaWNlX2NoYW5uZWxzQiYKJF9zY3JlZW5fcmVhZGVyX2Fubm91bmNlX25ld19tZXNzYWdlc0IqCihfZmlyc3RfY2xpY2tfcGFzc190aHJvdWdoX3doZW5fdW5mb2N1c2VkQiAKHl9jb21wYWN0X21lc3NhZ2VfZ3JvdXBfc3BhY2luZ0IjCiFfc2Nyb2xsX3RvX2JvdHRvbV9vbl9tZXNzYWdlX3NlbmRCGQoXX2RpbV9zdHJpa2V0aHJvdWdoX3RleHRCFwoVX3NlcXVlbnRpYWxfZmlsZV9zZW5kQh8KHV9tb2JpbGVfc3BsYXNoX3pvb21fYW5pbWF0aW9uSgQIKhArSgQIKxAsUh9hdHRhY2htZW50X21lZGlhX2RpbWVuc2lvbl9zaXplUhplbWJlZF9tZWRpYV9kaW1lbnNpb25fc2l6ZSJxChZBY2Nlc3NpYmlsaXR5T3ZlcnJpZGVzEhoKEmdpZl9hdXRvcGxheV9kaXJ0eRgBIAEoCBIbChNhbmltYXRlX2Vtb2ppX2RpcnR5GAIgASgIEh4KFmFuaW1hdGVfc3RpY2tlcnNfZGlydHkYAyABKAgq0gEKGkNoYW5uZWxUeXBpbmdJbmRpY2F0b3JNb2RlEi0KKUNIQU5ORUxfVFlQSU5HX0lORElDQVRPUl9NT0RFX1VOU1BFQ0lGSUVEEAASKQolQ0hBTk5FTF9UWVBJTkdfSU5ESUNBVE9SX01PREVfQVZBVEFSUxABEjAKLENIQU5ORUxfVFlQSU5HX0lORElDQVRPUl9NT0RFX0lORElDQVRPUl9PTkxZEAISKAokQ0hBTk5FTF9UWVBJTkdfSU5ESUNBVE9SX01PREVfSElEREVOEAMqqwEKFERtTWVzc2FnZVByZXZpZXdNb2RlEicKI0RNX01FU1NBR0VfUFJFVklFV19NT0RFX1VOU1BFQ0lGSUVEEAASHwobRE1fTUVTU0FHRV9QUkVWSUVXX01PREVfQUxMEAESJwojRE1fTUVTU0FHRV9QUkVWSUVXX01PREVfVU5SRUFEX09OTFkQAhIgChxETV9NRVNTQUdFX1BSRVZJRVdfTU9ERV9OT05FEAMqbAoOSGRyRGlzcGxheU1vZGUSIAocSERSX0RJU1BMQVlfTU9ERV9VTlNQRUNJRklFRBAAEhkKFUhEUl9ESVNQTEFZX01PREVfRlVMTBABEh0KGUhEUl9ESVNQTEFZX01PREVfU1RBTkRBUkQQAmIGcHJvdG8z");

/**
 * @generated from message fluxer.user.preferences.v1.AccessibilitySettings
 */
export type AccessibilitySettings = Message<"fluxer.user.preferences.v1.AccessibilitySettings"> & {
  /**
   * @generated from field: optional double saturation_factor = 1;
   */
  saturationFactor?: number | undefined;

  /**
   * @generated from field: bool always_underline_links = 2;
   */
  alwaysUnderlineLinks: boolean;

  /**
   * @generated from field: optional bool enable_text_selection = 3;
   */
  enableTextSelection?: boolean | undefined;

  /**
   * @generated from field: optional bool show_message_send_button = 4;
   */
  showMessageSendButton?: boolean | undefined;

  /**
   * @generated from field: optional bool show_textarea_focus_ring = 5;
   */
  showTextareaFocusRing?: boolean | undefined;

  /**
   * @generated from field: bool hide_keyboard_hints = 6;
   */
  hideKeyboardHints: boolean;

  /**
   * @generated from field: optional bool escape_exits_keyboard_mode = 7;
   */
  escapeExitsKeyboardMode?: boolean | undefined;

  /**
   * @generated from field: optional bool sync_reduced_motion_with_system = 8;
   */
  syncReducedMotionWithSystem?: boolean | undefined;

  /**
   * @generated from field: optional bool reduced_motion_override = 9;
   */
  reducedMotionOverride?: boolean | undefined;

  /**
   * @generated from field: optional double message_group_spacing = 10;
   */
  messageGroupSpacing?: number | undefined;

  /**
   * @generated from field: optional double message_gutter = 11;
   */
  messageGutter?: number | undefined;

  /**
   * @generated from field: optional double font_size = 12;
   */
  fontSize?: number | undefined;

  /**
   * @generated from field: optional bool show_user_avatars_in_compact_mode = 13;
   */
  showUserAvatarsInCompactMode?: boolean | undefined;

  /**
   * @generated from field: bool mobile_sticker_animation_overridden = 14;
   */
  mobileStickerAnimationOverridden: boolean;

  /**
   * @generated from field: bool mobile_gif_autoplay_overridden = 15;
   */
  mobileGifAutoplayOverridden: boolean;

  /**
   * @generated from field: bool mobile_animate_emoji_overridden = 16;
   */
  mobileAnimateEmojiOverridden: boolean;

  /**
   * @generated from field: optional int32 mobile_sticker_animation_value = 17;
   */
  mobileStickerAnimationValue?: number | undefined;

  /**
   * @generated from field: optional bool mobile_gif_autoplay_value = 18;
   */
  mobileGifAutoplayValue?: boolean | undefined;

  /**
   * @generated from field: optional bool mobile_animate_emoji_value = 19;
   */
  mobileAnimateEmojiValue?: boolean | undefined;

  /**
   * @generated from field: bool auto_send_klipy_gifs = 20;
   */
  autoSendKlipyGifs: boolean;

  /**
   * @generated from field: optional bool show_gif_button = 21;
   */
  showGifButton?: boolean | undefined;

  /**
   * @generated from field: optional bool show_memes_button = 22;
   */
  showMemesButton?: boolean | undefined;

  /**
   * @generated from field: optional bool show_stickers_button = 23;
   */
  showStickersButton?: boolean | undefined;

  /**
   * @generated from field: optional bool show_emoji_button = 24;
   */
  showEmojiButton?: boolean | undefined;

  /**
   * @generated from field: optional bool show_media_favorite_button = 25;
   */
  showMediaFavoriteButton?: boolean | undefined;

  /**
   * @generated from field: optional bool show_media_download_button = 26;
   */
  showMediaDownloadButton?: boolean | undefined;

  /**
   * @generated from field: optional bool show_media_delete_button = 27;
   */
  showMediaDeleteButton?: boolean | undefined;

  /**
   * @generated from field: optional bool show_suppress_embeds_button = 28;
   */
  showSuppressEmbedsButton?: boolean | undefined;

  /**
   * @generated from field: optional bool show_gif_indicator = 29;
   */
  showGifIndicator?: boolean | undefined;

  /**
   * @generated from field: optional bool show_attachment_expiry_indicator = 30;
   */
  showAttachmentExpiryIndicator?: boolean | undefined;

  /**
   * @generated from field: optional bool use_browser_locale_for_time_format = 31;
   */
  useBrowserLocaleForTimeFormat?: boolean | undefined;

  /**
   * @generated from field: fluxer.user.preferences.v1.ChannelTypingIndicatorMode channel_typing_indicator_mode = 32;
   */
  channelTypingIndicatorMode: ChannelTypingIndicatorMode;

  /**
   * @generated from field: optional bool show_selected_channel_typing_indicator = 33;
   */
  showSelectedChannelTypingIndicator?: boolean | undefined;

  /**
   * @generated from field: optional bool show_message_action_bar = 34;
   */
  showMessageActionBar?: boolean | undefined;

  /**
   * @generated from field: optional bool show_message_action_bar_quick_reactions = 35;
   */
  showMessageActionBarQuickReactions?: boolean | undefined;

  /**
   * @generated from field: optional bool show_message_action_bar_shift_expand = 36;
   */
  showMessageActionBarShiftExpand?: boolean | undefined;

  /**
   * @generated from field: optional bool show_message_action_bar_only_more_button = 37;
   */
  showMessageActionBarOnlyMoreButton?: boolean | undefined;

  /**
   * @generated from field: optional bool show_default_emojis_in_autocomplete = 38;
   */
  showDefaultEmojisInAutocomplete?: boolean | undefined;

  /**
   * @generated from field: optional bool show_custom_emojis_in_autocomplete = 39;
   */
  showCustomEmojisInAutocomplete?: boolean | undefined;

  /**
   * @generated from field: optional bool show_stickers_in_autocomplete = 40;
   */
  showStickersInAutocomplete?: boolean | undefined;

  /**
   * @generated from field: optional bool show_memes_in_autocomplete = 41;
   */
  showMemesInAutocomplete?: boolean | undefined;

  /**
   * @generated from field: optional bool voice_channel_join_requires_double_click = 44;
   */
  voiceChannelJoinRequiresDoubleClick?: boolean | undefined;

  /**
   * @generated from field: optional string custom_theme_css = 45;
   */
  customThemeCss?: string | undefined;

  /**
   * @generated from field: optional bool show_favorites = 46;
   */
  showFavorites?: boolean | undefined;

  /**
   * @generated from field: optional double zoom_level = 47;
   */
  zoomLevel?: number | undefined;

  /**
   * @generated from field: fluxer.user.preferences.v1.DmMessagePreviewMode dm_message_preview_mode = 48;
   */
  dmMessagePreviewMode: DmMessagePreviewMode;

  /**
   * @generated from field: optional bool enable_tts_command = 49;
   */
  enableTtsCommand?: boolean | undefined;

  /**
   * @generated from field: optional double tts_rate = 50;
   */
  ttsRate?: number | undefined;

  /**
   * @generated from field: optional bool show_faded_unread_on_muted_channels = 51;
   */
  showFadedUnreadOnMutedChannels?: boolean | undefined;

  /**
   * @generated from field: optional bool show_context_menu_shortcuts = 52;
   */
  showContextMenuShortcuts?: boolean | undefined;

  /**
   * @generated from field: optional bool confirm_before_starting_calls = 53;
   */
  confirmBeforeStartingCalls?: boolean | undefined;

  /**
   * @generated from field: fluxer.user.preferences.v1.HdrDisplayMode hdr_display_mode = 54;
   */
  hdrDisplayMode: HdrDisplayMode;

  /**
   * @generated from field: optional bool preserve_edit_draft = 55;
   */
  preserveEditDraft?: boolean | undefined;

  /**
   * @generated from field: optional bool stay_interactive_when_unfocused = 56;
   */
  stayInteractiveWhenUnfocused?: boolean | undefined;

  /**
   * @generated from field: optional bool confirm_before_joining_voice_channels = 57;
   */
  confirmBeforeJoiningVoiceChannels?: boolean | undefined;

  /**
   * @generated from field: optional bool screen_reader_announce_new_messages = 58;
   */
  screenReaderAnnounceNewMessages?: boolean | undefined;

  /**
   * @generated from field: optional bool first_click_pass_through_when_unfocused = 59;
   */
  firstClickPassThroughWhenUnfocused?: boolean | undefined;

  /**
   * @generated from field: optional double compact_message_group_spacing = 60;
   */
  compactMessageGroupSpacing?: number | undefined;

  /**
   * @generated from field: optional bool scroll_to_bottom_on_message_send = 61;
   */
  scrollToBottomOnMessageSend?: boolean | undefined;

  /**
   * @generated from field: optional bool dim_strikethrough_text = 62;
   */
  dimStrikethroughText?: boolean | undefined;

  /**
   * @generated from field: optional bool sequential_file_send = 63;
   */
  sequentialFileSend?: boolean | undefined;

  /**
   * @generated from field: optional bool mobile_splash_zoom_animation = 64;
   */
  mobileSplashZoomAnimation?: boolean | undefined;
};

/**
 * Describes the message fluxer.user.preferences.v1.AccessibilitySettings.
 * Use `create(AccessibilitySettingsSchema)` to create a new message.
 */
export const AccessibilitySettingsSchema: GenMessage<AccessibilitySettings> = /*@__PURE__*/
  messageDesc(file_fluxer_user_preferences_v1_accessibility, 0);

/**
 * @generated from message fluxer.user.preferences.v1.AccessibilityOverrides
 */
export type AccessibilityOverrides = Message<"fluxer.user.preferences.v1.AccessibilityOverrides"> & {
  /**
   * @generated from field: bool gif_autoplay_dirty = 1;
   */
  gifAutoplayDirty: boolean;

  /**
   * @generated from field: bool animate_emoji_dirty = 2;
   */
  animateEmojiDirty: boolean;

  /**
   * @generated from field: bool animate_stickers_dirty = 3;
   */
  animateStickersDirty: boolean;
};

/**
 * Describes the message fluxer.user.preferences.v1.AccessibilityOverrides.
 * Use `create(AccessibilityOverridesSchema)` to create a new message.
 */
export const AccessibilityOverridesSchema: GenMessage<AccessibilityOverrides> = /*@__PURE__*/
  messageDesc(file_fluxer_user_preferences_v1_accessibility, 1);

/**
 * @generated from enum fluxer.user.preferences.v1.ChannelTypingIndicatorMode
 */
export enum ChannelTypingIndicatorMode {
  /**
   * @generated from enum value: CHANNEL_TYPING_INDICATOR_MODE_UNSPECIFIED = 0;
   */
  UNSPECIFIED = 0,

  /**
   * @generated from enum value: CHANNEL_TYPING_INDICATOR_MODE_AVATARS = 1;
   */
  AVATARS = 1,

  /**
   * @generated from enum value: CHANNEL_TYPING_INDICATOR_MODE_INDICATOR_ONLY = 2;
   */
  INDICATOR_ONLY = 2,

  /**
   * @generated from enum value: CHANNEL_TYPING_INDICATOR_MODE_HIDDEN = 3;
   */
  HIDDEN = 3,
}

/**
 * Describes the enum fluxer.user.preferences.v1.ChannelTypingIndicatorMode.
 */
export const ChannelTypingIndicatorModeSchema: GenEnum<ChannelTypingIndicatorMode> = /*@__PURE__*/
  enumDesc(file_fluxer_user_preferences_v1_accessibility, 0);

/**
 * @generated from enum fluxer.user.preferences.v1.DmMessagePreviewMode
 */
export enum DmMessagePreviewMode {
  /**
   * @generated from enum value: DM_MESSAGE_PREVIEW_MODE_UNSPECIFIED = 0;
   */
  UNSPECIFIED = 0,

  /**
   * @generated from enum value: DM_MESSAGE_PREVIEW_MODE_ALL = 1;
   */
  ALL = 1,

  /**
   * @generated from enum value: DM_MESSAGE_PREVIEW_MODE_UNREAD_ONLY = 2;
   */
  UNREAD_ONLY = 2,

  /**
   * @generated from enum value: DM_MESSAGE_PREVIEW_MODE_NONE = 3;
   */
  NONE = 3,
}

/**
 * Describes the enum fluxer.user.preferences.v1.DmMessagePreviewMode.
 */
export const DmMessagePreviewModeSchema: GenEnum<DmMessagePreviewMode> = /*@__PURE__*/
  enumDesc(file_fluxer_user_preferences_v1_accessibility, 1);

/**
 * @generated from enum fluxer.user.preferences.v1.HdrDisplayMode
 */
export enum HdrDisplayMode {
  /**
   * @generated from enum value: HDR_DISPLAY_MODE_UNSPECIFIED = 0;
   */
  UNSPECIFIED = 0,

  /**
   * @generated from enum value: HDR_DISPLAY_MODE_FULL = 1;
   */
  FULL = 1,

  /**
   * @generated from enum value: HDR_DISPLAY_MODE_STANDARD = 2;
   */
  STANDARD = 2,
}

/**
 * Describes the enum fluxer.user.preferences.v1.HdrDisplayMode.
 */
export const HdrDisplayModeSchema: GenEnum<HdrDisplayMode> = /*@__PURE__*/
  enumDesc(file_fluxer_user_preferences_v1_accessibility, 2);
