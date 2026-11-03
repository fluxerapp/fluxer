---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: User settings Protobuf
description: The Protobuf messages and enumerations in the synced_preferences field of user settings.
---

The `synced_preferences` field of the [user settings object](/http-api/users/#user-settings-object) is a base64-encoded `fluxer.user.preferences.v1.SyncedPreferences` message with the client preferences an account shares between its devices. The API stores the snapshot and doesn't act on any field in it. The schema is in `packages/schema/proto/fluxer/user/preferences/v1`.

## Reading and writing the snapshot

[Modify current user settings](/http-api/users/settings/#modify-current-user-settings) replaces the whole snapshot. To change one preference, decode the stored snapshot, change it and send all of it back. Sending null or the empty string clears it, and the API returns the empty string when nothing is stored. Each change goes to every session of the account in [User Settings Update](/gateway/events/#user-settings-update).

The API accepts standard or URL-safe base64, with or without padding, and returns standard base64 with padding. The API re-encodes what you send and keeps unknown fields. A snapshot with only default values comes back as the empty string.

An invalid snapshot returns 400 with errors at the path `synced_preferences`.

| Condition | Status and code | Element codes |
| --- | --- | --- |
| Encoded string longer than 699052 characters | 400 `INVALID_FORM_BODY` | `CONTENT_EXCEEDS_MAX_LENGTH` and `INVALID_FORMAT` |
| Encoded string outside the base64 alphabet | 400 `INVALID_FORM_BODY` | `INVALID_FORMAT` |
| Decoded message above 524288 bytes (512 KiB) | 400 `VALIDATION_ERROR` | `TOO_LARGE` |
| Bytes that don't decode as `SyncedPreferences` | 400 `VALIDATION_ERROR` | `INVALID_FORMAT` |
## Synced preferences object

A field marked `?` is absent until a client sets it. Every other scalar field is 0, false or empty when unset. An enumeration field has the type `int32` and links to its values.

`SyncedPreferences` is the root message.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| accessibility? | [accessibility settings](#accessibility-settings-object) object | Display, motion, media and other accessibility preferences |
| accessibility_overrides? | [accessibility overrides](#accessibility-overrides-object) object | Media setting dirty flags |
| textual_preview? | [textual preview settings](#textual-preview-settings-object) object | Textual attachment preview preferences |
| emoji_picker? | [emoji picker state](#emoji-picker-state-object) object | Emoji picker usage and favorites |
| sticker_picker? | [sticker picker state](#sticker-picker-state-object) object | Sticker picker usage and favorites |
| memes_picker? | [memes picker state](#memes-picker-state-object) object | Memes picker usage and favorites |
| emoji? | [emoji state](#emoji-state-object) object | Emoji skin tone |
| emoji_sticker_layout? | [emoji and sticker layout settings](#emoji-and-sticker-layout-settings-object) object | Emoji and sticker picker layout |
| favorite_gifs? | [favorite GIF settings](#favorite-gif-settings-object) object | Favorite GIFs |
| favorites? | [favorites state](#favorites-state-object) object | Favorite channels and their categories |
| recent_mentions? | [recent mentions settings](#recent-mentions-settings-object) object | Recent mentions filters |
| sidebar? | [sidebar preferences](#sidebar-preferences-object) object | Direct message sidebar state |
| member_list? | [member list state](#member-list-state-object) object | Member list visibility |
| unread_channels? | [unread channels state](#unread-channels-state-object) object | Collapsed unread channels |
| mention_frecency? | [mention frecency state](#mention-frecency-state-object) object | Mention autocomplete ranking per guild |
| nagbars? | [nagbar dismissals](#nagbar-dismissals-object) object | Dismissed notices |
| dismissed_upsells? | [dismissed upsells](#dismissed-upsells-object) object | Dismissed upsells |
| guild_nsfw_agreements? | [guild mature content agreements](#guild-mature-content-agreements-object) object | Accepted mature content notices |
| whats_new? | [what's new state](#whats-new-state-object) object | Last dismissed update entry |
| privacy? | [privacy preferences](#privacy-preferences-object) object | Client privacy preferences |
| local_spam_overrides? | [local user spam overrides](#local-user-spam-overrides-object) object | Local spammer overrides |
| sanitize_urls | bool | Whether to strip tracking parameters from URLs in outgoing messages |
| sound? | [sound settings](#sound-settings-object) object | Sound preferences |
| spellcheck? | [spellcheck settings](#spellcheck-settings-object) object | Spellcheck preferences |
| search_engines? | [search engine settings](#search-engine-settings-object) object | Search and translation providers |
| permission_layout? | [permission layout settings](#permission-layout-settings-object) object | Permission editor layout |
| guild_member_layout? | [guild member layout settings](#guild-member-layout-settings-object) object | Guild member view layout |
| guild_folders? | [guild folder expanded state](#guild-folder-expanded-state-object) object | Expanded guild folders |
| hidden_guild_buttons? | [hidden guild list buttons](#hidden-guild-list-buttons-object) object | Hidden guild list buttons |
| keyboard_mode_intro? | [keyboard mode intro state](#keyboard-mode-intro-state-object) object | Keyboard mode introduction state |
| input_monitoring? | [input monitoring prompts state](#input-monitoring-prompts-state-object) object | Input monitoring prompt state |
| voice_prompts? | [voice prompts state](#voice-prompts-state-object) object | Skipped voice confirmations |
| sudo_prompt? | [sudo prompt state](#sudo-prompt-state-object) object | Last sudo verification method |
| keybinds? | [keybind settings](#keybind-settings-object) object | Custom keybinds and transmit mode |
| chat_input? | [chat input settings](#chat-input-settings-object) object | Message composer preferences |
| save_camera_uploads_to_device? | bool | Whether camera uploads are also saved to the device |
| double_tap_reaction? | [reaction emoji](#reaction-emoji-object) object | Emoji that a double tap on a message adds |
| channel_frecency? | [channel frecency state](#channel-frecency-state-object) object | Channel usage for ranking channel search |
| announcement_prompts? | [announcement prompts state](#announcement-prompts-state-object) object | Hidden announcement channel prompts |
| double_tap_action | int32 | [Double tap action](#double-tap-actions) on a message |
| swipe_right_to_left_action | int32 | [Swipe right to left action](#swipe-right-to-left-actions) in a channel |

## Accessibility settings object

Display, motion, message, media, voice and interaction preferences.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| saturation_factor? | double | Color saturation multiplier, where 1 is unchanged |
| always_underline_links | bool | Whether links are always underlined |
| enable_text_selection? | bool | Whether message text can be selected |
| show_message_send_button? | bool | Whether the composer shows a send button |
| show_textarea_focus_ring? | bool | Whether the composer shows a focus ring |
| hide_keyboard_hints | bool | Whether keyboard shortcut hints are hidden |
| escape_exits_keyboard_mode? | bool | Whether Escape exits keyboard navigation mode |
| sync_reduced_motion_with_system? | bool | Whether reduced motion follows the operating system |
| reduced_motion_override? | bool | Reduced motion setting, used when it doesn't follow the operating system |
| message_group_spacing? | double | Spacing between message groups in CSS pixels |
| message_gutter? | double | Message gutter width in CSS pixels |
| font_size? | double | Message font size in CSS pixels |
| show_user_avatars_in_compact_mode? | bool | Whether compact mode shows avatars |
| mobile_sticker_animation_overridden | bool | Whether the mobile sticker animation value is used |
| mobile_gif_autoplay_overridden | bool | Whether the mobile GIF autoplay value is used |
| mobile_animate_emoji_overridden | bool | Whether the mobile emoji animation value is used |
| mobile_sticker_animation_value? | int32 | Mobile [sticker animation setting](/http-api/users/#sticker-animation-settings) |
| mobile_gif_autoplay_value? | bool | Mobile GIF autoplay |
| mobile_animate_emoji_value? | bool | Mobile emoji animation |
| auto_send_klipy_gifs | bool | Whether picking a GIF sends it immediately |
| show_gif_button? | bool | Whether the composer shows the GIF button |
| show_memes_button? | bool | Whether the composer shows the memes button |
| show_stickers_button? | bool | Whether the composer shows the sticker button |
| show_emoji_button? | bool | Whether the composer shows the emoji button |
| show_media_favorite_button? | bool | Whether media shows the favorite button |
| show_media_download_button? | bool | Whether media shows the download button |
| show_media_delete_button? | bool | Whether media shows the delete button |
| show_suppress_embeds_button? | bool | Whether messages show the suppress embeds button |
| show_gif_indicator? | bool | Whether GIFs show a GIF indicator |
| show_attachment_expiry_indicator? | bool | Whether attachments show their expiry indicator |
| use_browser_locale_for_time_format? | bool | Whether times use the browser locale |
| channel_typing_indicator_mode | int32 | [Channel typing indicator mode](#channel-typing-indicator-modes) |
| show_selected_channel_typing_indicator? | bool | Whether the selected channel shows its typing indicator |
| show_message_action_bar? | bool | Whether messages show the action bar |
| show_message_action_bar_quick_reactions? | bool | Whether the action bar shows quick reactions |
| show_message_action_bar_shift_expand? | bool | Whether holding Shift expands the action bar |
| show_message_action_bar_only_more_button? | bool | Whether the action bar shows only the more button |
| show_default_emojis_in_autocomplete? | bool | Whether autocomplete includes standard emoji |
| show_custom_emojis_in_autocomplete? | bool | Whether autocomplete includes custom emoji |
| show_stickers_in_autocomplete? | bool | Whether autocomplete includes stickers |
| show_memes_in_autocomplete? | bool | Whether autocomplete includes memes |
| voice_channel_join_requires_double_click? | bool | Whether joining a voice channel needs a double click |
| custom_theme_css? | string | CSS of the synced custom theme |
| show_favorites? | bool | Whether the favorites section is shown |
| zoom_level? | double | App zoom multiplier, where 1 is unscaled |
| dm_message_preview_mode | int32 | [Direct message preview mode](#direct-message-preview-modes) |
| enable_tts_command? | bool | Whether the text-to-speech command is on |
| tts_rate? | double | Text-to-speech rate multiplier, where 1 is normal |
| show_faded_unread_on_muted_channels? | bool | Whether muted channels show a faded unread indicator |
| show_context_menu_shortcuts? | bool | Whether context menus show keyboard shortcuts |
| confirm_before_starting_calls? | bool | Whether starting a call asks for confirmation |
| hdr_display_mode | int32 | [HDR display mode](#hdr-display-modes) |
| preserve_edit_draft? | bool | Whether canceling an edit keeps its draft |
| stay_interactive_when_unfocused? | bool | Whether animation and interaction continue while the app is unfocused |
| confirm_before_joining_voice_channels? | bool | Whether joining a voice channel asks for confirmation |
| screen_reader_announce_new_messages? | bool | Whether a screen reader announces new messages |
| first_click_pass_through_when_unfocused? | bool | Whether the first click on an unfocused app also activates the control |
| compact_message_group_spacing? | double | Spacing between compact message groups in CSS pixels |
| scroll_to_bottom_on_message_send? | bool | Whether sending a message scrolls to the bottom |
| dim_strikethrough_text? | bool | Whether strikethrough text is dimmed |
| sequential_file_send? | bool | Whether multiple files are sent one message at a time |
| mobile_splash_zoom_animation? | bool | Whether the mobile splash screen plays its zoom animation |
| message_double_click_actions? | bool | Whether double-clicking a message edits your own message or replies to others |

### Channel typing indicator modes

| Value | Name | Description |
| --- | --- | --- |
| 0 | CHANNEL_TYPING_INDICATOR_MODE_UNSPECIFIED | No mode is set |
| 1 | CHANNEL_TYPING_INDICATOR_MODE_AVATARS | Show typing users as avatars |
| 2 | CHANNEL_TYPING_INDICATOR_MODE_INDICATOR_ONLY | Show the indicator without avatars |
| 3 | CHANNEL_TYPING_INDICATOR_MODE_HIDDEN | Hide the indicator |

### Direct message preview modes

| Value | Name | Description |
| --- | --- | --- |
| 0 | DM_MESSAGE_PREVIEW_MODE_UNSPECIFIED | No mode is set |
| 1 | DM_MESSAGE_PREVIEW_MODE_ALL | Preview all direct messages |
| 2 | DM_MESSAGE_PREVIEW_MODE_UNREAD_ONLY | Preview only unread direct messages |
| 3 | DM_MESSAGE_PREVIEW_MODE_NONE | Hide previews |

### HDR display modes

| Value | Name | Description |
| --- | --- | --- |
| 0 | HDR_DISPLAY_MODE_UNSPECIFIED | No mode is set |
| 1 | HDR_DISPLAY_MODE_FULL | Show HDR media at full range |
| 2 | HDR_DISPLAY_MODE_STANDARD | Limit HDR media to standard dynamic range |

## Accessibility overrides object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| gif_autoplay_dirty | bool | GIF autoplay dirty flag |
| animate_emoji_dirty | bool | Emoji animation dirty flag |
| animate_stickers_dirty | bool | Sticker animation dirty flag |

## Textual preview settings object

Line wrapping in the inline preview of a text attachment.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| wrap_text | bool | Whether long lines wrap |

## Usage stat object

How often and how recently one emoji, sticker or meme was used.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| count | uint32 | Number of uses |
| last_used_ms | int64 | Unix time in milliseconds of the last use |

## Emoji picker state object

Emoji usage, favorites and collapsed categories. Fluxer clients key emoji as `unicode:<name>` or `custom:<guild id>:<emoji id>`.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| usage | map[string, [usage stat](#usage-stat-object) object] | Usage keyed by emoji key |
| favorite_emoji_ids | array[string] | Favorite emoji keys in display order |
| collapsed_category_ids | array[string] | Collapsed category IDs |

## Sticker picker state object

Sticker usage, favorites and collapsed categories.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| usage | map[string, [usage stat](#usage-stat-object) object] | Usage keyed by sticker ID |
| favorite_sticker_ids | array[string] | Favorite sticker IDs in display order |
| collapsed_category_ids | array[string] | Collapsed category IDs |

## Memes picker state object

Meme usage, favorites and collapsed categories. The memes are in the account's [memes](/http-api/memes/).

### Structure

| Field | Type | Description |
| --- | --- | --- |
| usage | map[string, [usage stat](#usage-stat-object) object] | Usage keyed by meme ID |
| favorite_meme_ids | array[string] | Favorite meme IDs in display order |
| collapsed_category_ids | array[string] | Collapsed category IDs |

## Emoji state object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| skin_tone | string | Unicode skin tone modifier (U+1F3FB to U+1F3FF), or empty for the default |

## Emoji and sticker layout settings object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| emoji_layout | int32 | [Emoji picker layout](#emoji-picker-layouts) |
| sticker_view_mode | int32 | [Sticker picker view mode](#sticker-picker-view-modes) |

### Emoji picker layouts

| Value | Name | Description |
| --- | --- | --- |
| 0 | EMOJI_PICKER_LAYOUT_UNSPECIFIED | No layout is set |
| 1 | EMOJI_PICKER_LAYOUT_LIST | List layout |
| 2 | EMOJI_PICKER_LAYOUT_GRID | Grid layout |

### Sticker picker view modes

| Value | Name | Description |
| --- | --- | --- |
| 0 | STICKER_PICKER_VIEW_MODE_UNSPECIFIED | No view mode is set |
| 1 | STICKER_PICKER_VIEW_MODE_COZY | Roomy layout |
| 2 | STICKER_PICKER_VIEW_MODE_COMPACT | Compact layout |

## Favorite GIF settings object

Fluxer clients keep favorite GIFs here, or in the account's [memes](/http-api/memes/) when `save_as_saved_media` is true.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| entries | array[[favorite GIF entry](#favorite-gif-entry-object) object] | Favorite GIFs in display order |
| save_as_saved_media | bool | Whether favoriting a GIF saves it as a meme |
| seen_first_time_prompt | bool | Whether the first-time favorite prompt was shown |

## Favorite GIF entry object

One favorite GIF, in the same format as a [resolved GIF entry](/http-api/memes/#resolved-gif-entry-object).

### Structure

| Field | Type | Description |
| --- | --- | --- |
| url | string | Source URL of the GIF |
| proxy_url | string | [Media Proxy](/media-proxy/overview/) URL of the preview |
| width | uint32 | Preview width in pixels, or 0 if unknown |
| height | uint32 | Preview height in pixels, or 0 if unknown |
| media | map[string, [favorite GIF media format](#favorite-gif-media-format-object) object] | Formats keyed by [format name](/http-api/memes/#gif-media-format-names) |
| content_type | string | Media type of the preview, or empty for `image/gif` |
| placeholder | string | Thumbhash placeholder, or empty |

## Favorite GIF media format object

One encoding of a favorite GIF, in the same format as a [GIF media format](/http-api/memes/#gif-media-format-object).

### Structure

| Field | Type | Description |
| --- | --- | --- |
| src | string | Direct URL of this format |
| proxy_src | string | [Media Proxy](/media-proxy/overview/) URL of this format |
| width | uint32 | Width in pixels |
| height | uint32 | Height in pixels |

## Favorites state object

Favorite channels and the categories they're grouped in. They aren't guild categories.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| channels | array[[favorite channel](#favorite-channel-object) object] | Favorite channels |
| categories | array[[favorite category](#favorite-category-object) object] | Favorite categories |
| collapsed_category_ids | array[string] | Collapsed favorite category IDs |
| hide_muted_channels | bool | Whether muted channels are hidden |
| muted | bool | Whether favorites are muted |

## Favorite channel object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| channel_id | string | The ID of the channel |
| guild_id | string | The ID of the guild |
| parent_id? | string | The `id` of the [favorite category](#favorite-category-object) it's in |
| position | int32 | Position in the favorites list |
| nickname? | string | Nickname the user gave the channel |

## Favorite category object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| id | string | Category ID chosen by the client, not a snowflake |
| name | string | Category name |
| position | int32 | Category position |

## Recent mentions settings object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| include_everyone? | bool | Whether to include `@everyone` mentions (true when absent) |
| include_roles? | bool | Whether to include role mentions (true when absent) |
| include_guilds? | bool | Whether to include mentions in guild channels (true when absent) |

## Sidebar preferences object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| inline_dms_collapsed | bool | Whether the inline direct message section is collapsed |
| show_collapsed_unread_dms_badge? | bool | Whether the collapsed section shows an unread badge (true when absent) |
| show_incoming_friend_request_badge? | bool | Whether the sidebar shows a friend request badge (true when absent) |

## Member list state object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| members_open? | bool | Whether the member list is open |

## Unread channels state object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| collapsed_channel_ids | array[string] | IDs of collapsed channels in the unread view |

## Mention frecency state object

How often and how recently the account mentioned each user, per guild, for ranking mention autocomplete.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| scopes | array[[mention frecency scope](#mention-frecency-scope-object) object] | Mention records per guild |

## Mention frecency scope object

The mention records for one guild, declared as `MentionFrecencyState.Scope`.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| guild_id | string | The ID of the guild |
| entries | array[[mention frecency entry](#mention-frecency-entry-object) object] | Mention records for the guild |

## Mention frecency entry object

Mentions of one user, declared as `MentionFrecencyState.Entry`.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| user_id | string | The ID of the mentioned user |
| count | uint32 | Number of mentions |
| last_at_ms | int64 | Unix time in milliseconds of the last mention |

## Channel frecency state object

How often and how recently the account opened each channel.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| usage | map[string, [channel frecency entry](#channel-frecency-entry-object) object] | Usage keyed by channel ID |

## Channel frecency entry object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| total_uses | uint32 | Number of uses |
| recent_uses_ms | array[int64] | Unix times in milliseconds of recent uses |

## Nagbar dismissals object

Each field is true when the account dismissed that notice. In the map fields, a missing key means the notice wasn't dismissed.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| ios_install | bool | iOS install notice |
| pwa_install | bool | Progressive web app install notice |
| push_notification | bool | Push notification notice |
| desktop_notification | bool | Desktop notification notice |
| premium_grace_period | bool | Premium grace period notice |
| premium_expired | bool | Premium expired notice |
| premium_onboarding | bool | Premium onboarding notice |
| premium_trial_expiring | bool | Premium trial expiring notice |
| gift_inventory | bool | Gift inventory notice |
| desktop_download | bool | Desktop download notice |
| guild_membership_cta | bool | Guild membership notice |
| pending_bulk_deletion | map[string, bool] | Pending bulk deletion notices, keyed by deletion ID |
| invites_disabled | map[string, bool] | Disabled invites notices, keyed by guild ID |
| guild_mfa_requirement | map[string, bool] | Guild MFA requirement notices, keyed by guild ID |

## Dismissed upsells object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| picker_premium | bool | Whether the premium upsell in the picker is dismissed |

## Guild mature content agreements object

The guilds, categories and channels where the account accepted the mature content notice, each accepted separately.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| agreed_channel_ids | array[string] | Channel IDs |
| agreed_guild_ids | array[string] | Guild IDs |
| agreed_category_ids | array[string] | Category IDs |

## What's new state object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| last_dismissed_entry_id? | string | ID of the last dismissed update entry |

## Privacy preferences object

These preferences don't change the privacy fields of the [user settings object](/http-api/users/#user-settings-object).

### Structure

| Field | Type | Description |
| --- | --- | --- |
| disable_stream_previews | bool | Whether the client stops uploading stream preview frames |
| show_active_now? | bool | Whether the active now view is shown |
| preupload_message_attachments? | bool | Whether attachments upload before the message is sent |

## Local user spam overrides object

Overrides for how the client shows the [SPAMMER public user flag](/http-api/users/#public-user-flags). They change only the display, and `not_spammer_user_ids` wins over both the other list and the flag.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| spammer_user_ids | array[string] | User IDs shown as spammers |
| not_spammer_user_ids | array[string] | User IDs shown as not spammers |

## Sound settings object

Volumes are percentages from 0 to 200, where 100 is the default.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| all_sounds_disabled | bool | Whether all sounds are off |
| master_volume? | double | Master volume |
| disabled_sounds | map[string, bool] | Disabled sounds keyed by [sound identifier](#sound-identifiers) |
| sound_overrides | map[string, double] | Volume per sound, keyed by [sound identifier](#sound-identifiers) |

### Sound identifiers

The API accepts any key. Fluxer clients use these.

| Value | Description |
| --- | --- |
| deaf | Deafen |
| undeaf | Undeafen |
| mute | Mute |
| unmute | Unmute |
| message | Message notification |
| direct-message | Direct message notification |
| same-channel-message | Message in the channel you're viewing |
| incoming-ring | Incoming call |
| user-join | Someone joins voice |
| user-leave | Someone leaves voice |
| user-move | Someone is moved in voice |
| viewer-join | Stream viewer joins |
| viewer-leave | Stream viewer leaves |
| voice-disconnect | Voice disconnects |
| camera-on | Camera on |
| camera-off | Camera off |
| screen-share-start | Screen share starts |
| screen-share-stop | Screen share stops |

## Spellcheck settings object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| enabled? | bool | Whether spellcheck is on |
| languages | array[string] | Spellcheck languages |
| personal_dictionary | array[string] | Words in the personal dictionary |
| auto_detect? | bool | Whether languages are detected automatically |
| engine? | string | Spellcheck engine (`auto`, `system` or `hunspell`) |

## Search engine settings object

Provider IDs that the client defines.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| text_search_engine_id? | string | Text search engine |
| reverse_image_search_engine_id? | string | Reverse image search engine |
| translation_provider_id? | string | Translation provider |

## Permission layout settings object

Layout of the [permission](/http-api/permissions/) editor.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| layout | int32 | [Permission layout mode](#permission-layout-modes) |
| grid | int32 | [Permission grid mode](#permission-grid-modes) |

### Permission layout modes

| Value | Name | Description |
| --- | --- | --- |
| 0 | PERMISSION_LAYOUT_MODE_UNSPECIFIED | No layout is set |
| 1 | PERMISSION_LAYOUT_MODE_COMFY | Roomy layout |
| 2 | PERMISSION_LAYOUT_MODE_DENSE | Dense layout |

### Permission grid modes

| Value | Name | Description |
| --- | --- | --- |
| 0 | PERMISSION_GRID_MODE_UNSPECIFIED | No grid mode is set |
| 1 | PERMISSION_GRID_MODE_SINGLE | One category at a time |
| 2 | PERMISSION_GRID_MODE_GRID | Categories in a grid |

## Guild member layout settings object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| mode | int32 | [Guild member view mode](#guild-member-view-modes) |

### Guild member view modes

| Value | Name | Description |
| --- | --- | --- |
| 0 | GUILD_MEMBER_VIEW_MODE_UNSPECIFIED | No view mode is set |
| 1 | GUILD_MEMBER_VIEW_MODE_TABLE | Table |
| 2 | GUILD_MEMBER_VIEW_MODE_GRID | Grid |

## Guild folder expanded state object

The folders themselves are `guild_folders` on the [user settings object](/http-api/users/#user-settings-object).

### Structure

| Field | Type | Description |
| --- | --- | --- |
| expanded_folder_ids | array[fixed64] | IDs of expanded [guild folders](/http-api/users/#guild-folder-object) |

## Hidden guild list buttons object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| download_button | bool | Whether the download button is hidden |
| help_button | bool | Whether the help button is hidden |

## Keyboard mode intro state object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| seen | bool | Whether the keyboard navigation introduction was shown |

## Input monitoring prompts state object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| seen_cta | bool | Whether the input monitoring prompt was shown |

## Voice prompts state object

Voice confirmations the account chose to skip.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| skip_hide_own_camera_confirm | bool | Skip confirming before hiding your own camera |
| skip_hide_own_screenshare_confirm | bool | Skip confirming before hiding your own screen share |
| skip_p2p_join_confirm | bool | Skip confirming before joining a peer-to-peer call |

## Announcement prompts state object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| hide_publish_nudge | bool | Whether the prompt to publish in an [announcement channel](/topics/announcement-channels/) is hidden |

## Sudo prompt state object

The verification method used last for [sudo mode](/http-api/users/mfa/#sudo-mode), which clients preselect next time.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| last_used_mfa_method? | int32 | [MFA method](#mfa-methods) |

### MFA methods

| Value | Name | Description |
| --- | --- | --- |
| 0 | MFA_METHOD_UNSPECIFIED | No method is recorded |
| 1 | MFA_METHOD_TOTP | Authenticator app (`totp`) |
| 3 | MFA_METHOD_WEBAUTHN | Security key or passkey (`webauthn`) |

## Keybind settings object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| custom_keybinds | array[[custom keybind](#custom-keybind-object) object] | Custom keybinds |
| transmit_mode | string | `voice_activity` (default) or `voice_push_to_talk` |
| push_to_talk_release_delay_ms? | uint32 | Push-to-talk release delay in milliseconds (20-2000) |

## Custom keybind object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| id | string | Keybind ID chosen by the client |
| action? | string | Action the client runs |
| combo? | [keybind combo](#keybind-combo-object) object | Input combination |
| enabled | bool | Whether the keybind is on |

## Keybind combo object

The key, modifiers and buttons that trigger a keybind.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| key | string | Key value |
| code? | string | Physical key code |
| ctrl_or_meta | bool | Whether Meta on macOS or Control elsewhere is needed |
| ctrl | bool | Whether Control is needed |
| alt | bool | Whether Alt is needed |
| shift | bool | Whether Shift is needed |
| meta | bool | Whether Meta is needed |
| global? | bool | Whether the combination works when the app is unfocused |
| enabled? | bool | Ignored for custom keybinds, which use the keybind's own `enabled` |
| modifier_only | bool | Whether the combination is only modifier keys |
| both_sides | bool | Whether either side of a paired modifier works |
| mouse_button? | uint32 | Mouse button number |
| gamepad_button? | uint32 | Gamepad button number |

## Chat input settings object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| convert_emoticons? | bool | Whether emoticons turn into emoji |

## Reaction emoji object

The same format as the [reaction emoji object](/gateway/events/#reaction-emoji-object).

### Structure

| Field | Type | Description |
| --- | --- | --- |
| id? | string | The ID of a custom emoji, absent for a Unicode emoji |
| name | string | The Unicode emoji, or the custom emoji's name |

## Double tap actions

What a double tap on a message does on mobile.

| Value | Name | Description |
| --- | --- | --- |
| 0 | DOUBLE_TAP_ACTION_UNSPECIFIED | No action is set, which Fluxer clients treat as react |
| 1 | DOUBLE_TAP_ACTION_REACT | React with `double_tap_reaction` |
| 2 | DOUBLE_TAP_ACTION_EDIT | Edit the message if it's your own |
| 3 | DOUBLE_TAP_ACTION_NONE | Do nothing |

## Swipe right to left actions

What a right to left swipe in a channel does on mobile.

| Value | Name | Description |
| --- | --- | --- |
| 0 | SWIPE_ACTION_UNSPECIFIED | No action is set |
| 1 | SWIPE_ACTION_CHANNEL_DETAILS | Open the channel details |
| 2 | SWIPE_ACTION_REPLY | Reply to the message |
