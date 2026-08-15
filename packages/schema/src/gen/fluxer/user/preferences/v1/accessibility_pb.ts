import type { GenEnum, GenFile, GenMessage } from "@bufbuild/protobuf/codegenv2";
import { enumDesc, fileDesc, messageDesc } from "@bufbuild/protobuf/codegenv2";
import type { Message } from "@bufbuild/protobuf";

/**
 * Describes the file fluxer/user/preferences/v1/accessibility.proto.
 */
export const file_fluxer_user_preferences_v1_accessibility: GenFile = /*@__PURE__*/
  fileDesc("Ci5mbHV4ZXIvdXNlci9wcmVmZXJlbmNlcy92MS9hY2Nlc3NpYmlsaXR5LnByb3RvEhpmbHV4ZXIudXNlci5wcmVmZXJlbmNlcy52MSLrIgoVQWNjZXNzaWJpbGl0eVNldHRpbmdzEh4KEXNhdHVyYXRpb25fZmFjdG9yGAEgASgBSACIAQESHgoWYWx3YXlzX3VuZGVybGluZV9saW5rcxgCIAEoCBIiChVlbmFibGVfdGV4dF9zZWxlY3Rpb24YAyABKAhIAYgBARIlChhzaG93X21lc3NhZ2Vfc2VuZF9idXR0b24YBCABKAhIAogBARIlChhzaG93X3RleHRhcmVhX2ZvY3VzX3JpbmcYBSABKAhIA4gBARIbChNoaWRlX2tleWJvYXJkX2hpbnRzGAYgASgIEicKGmVzY2FwZV9leGl0c19rZXlib2FyZF9tb2RlGAcgASgISASIAQESLAofc3luY19yZWR1Y2VkX21vdGlvbl93aXRoX3N5c3RlbRgIIAEoCEgFiAEBEiQKF3JlZHVjZWRfbW90aW9uX292ZXJyaWRlGAkgASgISAaIAQESIgoVbWVzc2FnZV9ncm91cF9zcGFjaW5nGAogASgBSAeIAQESGwoObWVzc2FnZV9ndXR0ZXIYCyABKAFICIgBARIWCglmb250X3NpemUYDCABKAFICYgBARIuCiFzaG93X3VzZXJfYXZhdGFyc19pbl9jb21wYWN0X21vZGUYDSABKAhICogBARIrCiNtb2JpbGVfc3RpY2tlcl9hbmltYXRpb25fb3ZlcnJpZGRlbhgOIAEoCBImCh5tb2JpbGVfZ2lmX2F1dG9wbGF5X292ZXJyaWRkZW4YDyABKAgSJwofbW9iaWxlX2FuaW1hdGVfZW1vamlfb3ZlcnJpZGRlbhgQIAEoCBIrCh5tb2JpbGVfc3RpY2tlcl9hbmltYXRpb25fdmFsdWUYESABKAVIC4gBARImChltb2JpbGVfZ2lmX2F1dG9wbGF5X3ZhbHVlGBIgASgISAyIAQESJwoabW9iaWxlX2FuaW1hdGVfZW1vamlfdmFsdWUYEyABKAhIDYgBARIcChRhdXRvX3NlbmRfa2xpcHlfZ2lmcxgUIAEoCBIcCg9zaG93X2dpZl9idXR0b24YFSABKAhIDogBARIeChFzaG93X21lbWVzX2J1dHRvbhgWIAEoCEgPiAEBEiEKFHNob3dfc3RpY2tlcnNfYnV0dG9uGBcgASgISBCIAQESHgoRc2hvd19lbW9qaV9idXR0b24YGCABKAhIEYgBARInChpzaG93X21lZGlhX2Zhdm9yaXRlX2J1dHRvbhgZIAEoCEgSiAEBEicKGnNob3dfbWVkaWFfZG93bmxvYWRfYnV0dG9uGBogASgISBOIAQESJQoYc2hvd19tZWRpYV9kZWxldGVfYnV0dG9uGBsgASgISBSIAQESKAobc2hvd19zdXBwcmVzc19lbWJlZHNfYnV0dG9uGBwgASgISBWIAQESHwoSc2hvd19naWZfaW5kaWNhdG9yGB0gASgISBaIAQESLQogc2hvd19hdHRhY2htZW50X2V4cGlyeV9pbmRpY2F0b3IYHiABKAhIF4gBARIvCiJ1c2VfYnJvd3Nlcl9sb2NhbGVfZm9yX3RpbWVfZm9ybWF0GB8gASgISBiIAQESXQodY2hhbm5lbF90eXBpbmdfaW5kaWNhdG9yX21vZGUYICABKA4yNi5mbHV4ZXIudXNlci5wcmVmZXJlbmNlcy52MS5DaGFubmVsVHlwaW5nSW5kaWNhdG9yTW9kZRIzCiZzaG93X3NlbGVjdGVkX2NoYW5uZWxfdHlwaW5nX2luZGljYXRvchghIAEoCEgZiAEBEiQKF3Nob3dfbWVzc2FnZV9hY3Rpb25fYmFyGCIgASgISBqIAQESNAonc2hvd19tZXNzYWdlX2FjdGlvbl9iYXJfcXVpY2tfcmVhY3Rpb25zGCMgASgISBuIAQESMQokc2hvd19tZXNzYWdlX2FjdGlvbl9iYXJfc2hpZnRfZXhwYW5kGCQgASgISByIAQESNQooc2hvd19tZXNzYWdlX2FjdGlvbl9iYXJfb25seV9tb3JlX2J1dHRvbhglIAEoCEgdiAEBEjAKI3Nob3dfZGVmYXVsdF9lbW9qaXNfaW5fYXV0b2NvbXBsZXRlGCYgASgISB6IAQESLwoic2hvd19jdXN0b21fZW1vamlzX2luX2F1dG9jb21wbGV0ZRgnIAEoCEgfiAEBEioKHXNob3dfc3RpY2tlcnNfaW5fYXV0b2NvbXBsZXRlGCggASgISCCIAQESJwoac2hvd19tZW1lc19pbl9hdXRvY29tcGxldGUYKSABKAhIIYgBARJXCh9hdHRhY2htZW50X21lZGlhX2RpbWVuc2lvbl9zaXplGCogASgOMi4uZmx1eGVyLnVzZXIucHJlZmVyZW5jZXMudjEuTWVkaWFEaW1lbnNpb25TaXplElIKGmVtYmVkX21lZGlhX2RpbWVuc2lvbl9zaXplGCsgASgOMi4uZmx1eGVyLnVzZXIucHJlZmVyZW5jZXMudjEuTWVkaWFEaW1lbnNpb25TaXplEjUKKHZvaWNlX2NoYW5uZWxfam9pbl9yZXF1aXJlc19kb3VibGVfY2xpY2sYLCABKAhIIogBARIdChBjdXN0b21fdGhlbWVfY3NzGC0gASgJSCOIAQESGwoOc2hvd19mYXZvcml0ZXMYLiABKAhIJIgBARIXCgp6b29tX2xldmVsGC8gASgBSCWIAQESUQoXZG1fbWVzc2FnZV9wcmV2aWV3X21vZGUYMCABKA4yMC5mbHV4ZXIudXNlci5wcmVmZXJlbmNlcy52MS5EbU1lc3NhZ2VQcmV2aWV3TW9kZRIfChJlbmFibGVfdHRzX2NvbW1hbmQYMSABKAhIJogBARIVCgh0dHNfcmF0ZRgyIAEoAUgniAEBEjAKI3Nob3dfZmFkZWRfdW5yZWFkX29uX211dGVkX2NoYW5uZWxzGDMgASgISCiIAQESKAobc2hvd19jb250ZXh0X21lbnVfc2hvcnRjdXRzGDQgASgISCmIAQESKgodY29uZmlybV9iZWZvcmVfc3RhcnRpbmdfY2FsbHMYNSABKAhIKogBARJEChBoZHJfZGlzcGxheV9tb2RlGDYgASgOMiouZmx1eGVyLnVzZXIucHJlZmVyZW5jZXMudjEuSGRyRGlzcGxheU1vZGUSIAoTcHJlc2VydmVfZWRpdF9kcmFmdBg3IAEoCEgriAEBEiwKH3N0YXlfaW50ZXJhY3RpdmVfd2hlbl91bmZvY3VzZWQYOCABKAhILIgBARIyCiVjb25maXJtX2JlZm9yZV9qb2luaW5nX3ZvaWNlX2NoYW5uZWxzGDkgASgISC2IAQESMAojc2NyZWVuX3JlYWRlcl9hbm5vdW5jZV9uZXdfbWVzc2FnZXMYOiABKAhILogBARI0CidmaXJzdF9jbGlja19wYXNzX3Rocm91Z2hfd2hlbl91bmZvY3VzZWQYOyABKAhIL4gBARIqCh1jb21wYWN0X21lc3NhZ2VfZ3JvdXBfc3BhY2luZxg8IAEoAUgwiAEBEi0KIHNjcm9sbF90b19ib3R0b21fb25fbWVzc2FnZV9zZW5kGD0gASgISDGIAQESIwoWZGltX3N0cmlrZXRocm91Z2hfdGV4dBg+IAEoCEgyiAEBEiEKFHNlcXVlbnRpYWxfZmlsZV9zZW5kGD8gASgISDOIAQESKQocbW9iaWxlX3NwbGFzaF96b29tX2FuaW1hdGlvbhhAIAEoCEg0iAEBQhQKEl9zYXR1cmF0aW9uX2ZhY3RvckIYChZfZW5hYmxlX3RleHRfc2VsZWN0aW9uQhsKGV9zaG93X21lc3NhZ2Vfc2VuZF9idXR0b25CGwoZX3Nob3dfdGV4dGFyZWFfZm9jdXNfcmluZ0IdChtfZXNjYXBlX2V4aXRzX2tleWJvYXJkX21vZGVCIgogX3N5bmNfcmVkdWNlZF9tb3Rpb25fd2l0aF9zeXN0ZW1CGgoYX3JlZHVjZWRfbW90aW9uX292ZXJyaWRlQhgKFl9tZXNzYWdlX2dyb3VwX3NwYWNpbmdCEQoPX21lc3NhZ2VfZ3V0dGVyQgwKCl9mb250X3NpemVCJAoiX3Nob3dfdXNlcl9hdmF0YXJzX2luX2NvbXBhY3RfbW9kZUIhCh9fbW9iaWxlX3N0aWNrZXJfYW5pbWF0aW9uX3ZhbHVlQhwKGl9tb2JpbGVfZ2lmX2F1dG9wbGF5X3ZhbHVlQh0KG19tb2JpbGVfYW5pbWF0ZV9lbW9qaV92YWx1ZUISChBfc2hvd19naWZfYnV0dG9uQhQKEl9zaG93X21lbWVzX2J1dHRvbkIXChVfc2hvd19zdGlja2Vyc19idXR0b25CFAoSX3Nob3dfZW1vamlfYnV0dG9uQh0KG19zaG93X21lZGlhX2Zhdm9yaXRlX2J1dHRvbkIdChtfc2hvd19tZWRpYV9kb3dubG9hZF9idXR0b25CGwoZX3Nob3dfbWVkaWFfZGVsZXRlX2J1dHRvbkIeChxfc2hvd19zdXBwcmVzc19lbWJlZHNfYnV0dG9uQhUKE19zaG93X2dpZl9pbmRpY2F0b3JCIwohX3Nob3dfYXR0YWNobWVudF9leHBpcnlfaW5kaWNhdG9yQiUKI191c2VfYnJvd3Nlcl9sb2NhbGVfZm9yX3RpbWVfZm9ybWF0QikKJ19zaG93X3NlbGVjdGVkX2NoYW5uZWxfdHlwaW5nX2luZGljYXRvckIaChhfc2hvd19tZXNzYWdlX2FjdGlvbl9iYXJCKgooX3Nob3dfbWVzc2FnZV9hY3Rpb25fYmFyX3F1aWNrX3JlYWN0aW9uc0InCiVfc2hvd19tZXNzYWdlX2FjdGlvbl9iYXJfc2hpZnRfZXhwYW5kQisKKV9zaG93X21lc3NhZ2VfYWN0aW9uX2Jhcl9vbmx5X21vcmVfYnV0dG9uQiYKJF9zaG93X2RlZmF1bHRfZW1vamlzX2luX2F1dG9jb21wbGV0ZUIlCiNfc2hvd19jdXN0b21fZW1vamlzX2luX2F1dG9jb21wbGV0ZUIgCh5fc2hvd19zdGlja2Vyc19pbl9hdXRvY29tcGxldGVCHQobX3Nob3dfbWVtZXNfaW5fYXV0b2NvbXBsZXRlQisKKV92b2ljZV9jaGFubmVsX2pvaW5fcmVxdWlyZXNfZG91YmxlX2NsaWNrQhMKEV9jdXN0b21fdGhlbWVfY3NzQhEKD19zaG93X2Zhdm9yaXRlc0INCgtfem9vbV9sZXZlbEIVChNfZW5hYmxlX3R0c19jb21tYW5kQgsKCV90dHNfcmF0ZUImCiRfc2hvd19mYWRlZF91bnJlYWRfb25fbXV0ZWRfY2hhbm5lbHNCHgocX3Nob3dfY29udGV4dF9tZW51X3Nob3J0Y3V0c0IgCh5fY29uZmlybV9iZWZvcmVfc3RhcnRpbmdfY2FsbHNCFgoUX3ByZXNlcnZlX2VkaXRfZHJhZnRCIgogX3N0YXlfaW50ZXJhY3RpdmVfd2hlbl91bmZvY3VzZWRCKAomX2NvbmZpcm1fYmVmb3JlX2pvaW5pbmdfdm9pY2VfY2hhbm5lbHNCJgokX3NjcmVlbl9yZWFkZXJfYW5ub3VuY2VfbmV3X21lc3NhZ2VzQioKKF9maXJzdF9jbGlja19wYXNzX3Rocm91Z2hfd2hlbl91bmZvY3VzZWRCIAoeX2NvbXBhY3RfbWVzc2FnZV9ncm91cF9zcGFjaW5nQiMKIV9zY3JvbGxfdG9fYm90dG9tX29uX21lc3NhZ2Vfc2VuZEIZChdfZGltX3N0cmlrZXRocm91Z2hfdGV4dEIXChVfc2VxdWVudGlhbF9maWxlX3NlbmRCHwodX21vYmlsZV9zcGxhc2hfem9vbV9hbmltYXRpb24icQoWQWNjZXNzaWJpbGl0eU92ZXJyaWRlcxIaChJnaWZfYXV0b3BsYXlfZGlydHkYASABKAgSGwoTYW5pbWF0ZV9lbW9qaV9kaXJ0eRgCIAEoCBIeChZhbmltYXRlX3N0aWNrZXJzX2RpcnR5GAMgASgIKtIBChpDaGFubmVsVHlwaW5nSW5kaWNhdG9yTW9kZRItCilDSEFOTkVMX1RZUElOR19JTkRJQ0FUT1JfTU9ERV9VTlNQRUNJRklFRBAAEikKJUNIQU5ORUxfVFlQSU5HX0lORElDQVRPUl9NT0RFX0FWQVRBUlMQARIwCixDSEFOTkVMX1RZUElOR19JTkRJQ0FUT1JfTU9ERV9JTkRJQ0FUT1JfT05MWRACEigKJENIQU5ORUxfVFlQSU5HX0lORElDQVRPUl9NT0RFX0hJRERFThADKnoKEk1lZGlhRGltZW5zaW9uU2l6ZRIkCiBNRURJQV9ESU1FTlNJT05fU0laRV9VTlNQRUNJRklFRBAAEh4KGk1FRElBX0RJTUVOU0lPTl9TSVpFX1NNQUxMEAESHgoaTUVESUFfRElNRU5TSU9OX1NJWkVfTEFSR0UQAiqrAQoURG1NZXNzYWdlUHJldmlld01vZGUSJwojRE1fTUVTU0FHRV9QUkVWSUVXX01PREVfVU5TUEVDSUZJRUQQABIfChtETV9NRVNTQUdFX1BSRVZJRVdfTU9ERV9BTEwQARInCiNETV9NRVNTQUdFX1BSRVZJRVdfTU9ERV9VTlJFQURfT05MWRACEiAKHERNX01FU1NBR0VfUFJFVklFV19NT0RFX05PTkUQAypsCg5IZHJEaXNwbGF5TW9kZRIgChxIRFJfRElTUExBWV9NT0RFX1VOU1BFQ0lGSUVEEAASGQoVSERSX0RJU1BMQVlfTU9ERV9GVUxMEAESHQoZSERSX0RJU1BMQVlfTU9ERV9TVEFOREFSRBACYgZwcm90bzM");

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
   * @generated from field: fluxer.user.preferences.v1.MediaDimensionSize attachment_media_dimension_size = 42;
   */
  attachmentMediaDimensionSize: MediaDimensionSize;

  /**
   * @generated from field: fluxer.user.preferences.v1.MediaDimensionSize embed_media_dimension_size = 43;
   */
  embedMediaDimensionSize: MediaDimensionSize;

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
 * @generated from enum fluxer.user.preferences.v1.MediaDimensionSize
 */
export enum MediaDimensionSize {
  /**
   * @generated from enum value: MEDIA_DIMENSION_SIZE_UNSPECIFIED = 0;
   */
  UNSPECIFIED = 0,

  /**
   * @generated from enum value: MEDIA_DIMENSION_SIZE_SMALL = 1;
   */
  SMALL = 1,

  /**
   * @generated from enum value: MEDIA_DIMENSION_SIZE_LARGE = 2;
   */
  LARGE = 2,
}

/**
 * Describes the enum fluxer.user.preferences.v1.MediaDimensionSize.
 */
export const MediaDimensionSizeSchema: GenEnum<MediaDimensionSize> = /*@__PURE__*/
  enumDesc(file_fluxer_user_preferences_v1_accessibility, 1);

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
  enumDesc(file_fluxer_user_preferences_v1_accessibility, 2);

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
  enumDesc(file_fluxer_user_preferences_v1_accessibility, 3);
