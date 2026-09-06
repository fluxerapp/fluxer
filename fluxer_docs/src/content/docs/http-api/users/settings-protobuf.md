---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: User settings Protobuf
description: The client preference snapshot in the user settings object.
---

A synced preferences snapshot holds the client settings an account shares between its devices. It travels as one base64-encoded `fluxer.user.preferences.v1.SyncedPreferences` message in the `synced_preferences` field of the [user settings object](/http-api/users/#user-settings-object). Fluxer stores it and interprets no field on this page.

## Reading and writing the snapshot

Decode the string before reading any preference, and encode a valid `SyncedPreferences` message when writing one. [Modify current user settings](/http-api/users/settings/#modify-current-user-settings) takes the complete snapshot every time.

The empty string in a response means nothing is stored. Sending null clears the stored snapshot, and so does sending the empty string. A stored snapshot reaches the account's other sessions through [User Settings Update](/gateway/events/#user-settings-update).

Fluxer decodes every submitted snapshot and re-encodes it in canonical form before storing it, so a read can return a different string from the one submitted. Known fields are emitted in ascending field number order, and an unrecognised field number is preserved and re-emitted after them. Enums here are open, and an unassigned numeric value survives the round trip. When every known field holds its zero value and no unrecognised field is present, the snapshot encodes to zero bytes and is stored as the empty string.

A submission may use either the standard or the URL-safe base64 alphabet, with or without padding. Fluxer always returns the standard alphabet with padding.

## How to read the type column

Types here are the declared Protobuf types. A field whose declared type is an enumeration appears as `int32`, its wire representation, and the Description column links the enumeration.

A field without the optional marker always decodes and holds its Protobuf zero value: false for a bool, 0 for a numeric type, the empty string, an empty repeated field, or an empty map. A field with the optional marker tracks presence and is absent until a value is stored. Every singular message field tracks presence the same way, so a preference group is absent from the snapshot until a client writes to it.

Fluxer applies no bound to any individual field of the message, so every length, range, and enumeration membership below describes what the first-party client writes and reads.

## Rejection grounds

Fluxer reports every entry in the resulting `errors` array against the path `synced_preferences`. It checks the encoded string first.

| Condition | Status and code | Element codes |
| --- | --- | --- |
| Encoded string longer than 349528 characters | 400 `INVALID_FORM_BODY` | `CONTENT_EXCEEDS_MAX_LENGTH` and `INVALID_FORMAT` |
| Encoded string outside the base64 alphabet | 400 `INVALID_FORM_BODY` | `INVALID_FORMAT` |
| Decoded message above 262144 bytes | 400 `INVALID_FORM_BODY` | `TOO_LARGE` |
| Bytes that do not decode as `SyncedPreferences` | 400 `INVALID_FORM_BODY` | `INVALID_FORMAT` |

An over-length string draws two entries for the one path.

## Synced preferences object

The `SyncedPreferences` message is the root of the snapshot. Every field is a preference group defined in its own section, except `sanitize_urls` and `save_camera_uploads_to_device`, which are bools. Field numbers are allocated in blocks: 1 to 3, 20 to 25, 40 to 45, 60 to 63, 80 to 82, and 100 to 113. Never derive a field number from a field's position in this table.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| accessibility? | [accessibility settings](#accessibility-settings-object) object | Accessibility, display, motion, media, and interaction preferences |
| accessibility_overrides? | [accessibility overrides](#accessibility-overrides-object) object | Dirty flags for three media settings, with no live consumer |
| textual_preview? | [textual preview settings](#textual-preview-settings-object) object | Textual preview wrapping preferences |
| emoji_picker? | [emoji picker state](#emoji-picker-state-object) object | Emoji picker usage, favourites, and collapsed categories |
| sticker_picker? | [sticker picker state](#sticker-picker-state-object) object | Sticker picker usage, favourites, and collapsed categories |
| memes_picker? | [memes picker state](#memes-picker-state-object) object | Meme picker usage, favourites, and collapsed categories |
| emoji? | [emoji state](#emoji-state-object) object | Emoji skin tone preference |
| emoji_sticker_layout? | [emoji and sticker layout settings](#emoji-and-sticker-layout-settings-object) object | Emoji and sticker picker layout preferences |
| favorite_gifs?<sup>1</sup> | [favourite GIF settings](#favourite-gif-settings-object) object | Saved favourite GIF entries |
| favorites? | [favourites state](#favourites-state-object) object | Favourite channel and category layout |
| recent_mentions? | [recent mentions settings](#recent-mentions-settings-object) object | Recent mention inclusion filters |
| sidebar? | [sidebar preferences](#sidebar-preferences-object) object | Direct message sidebar state |
| member_list? | [member list state](#member-list-state-object) object | Member list visibility state |
| unread_channels? | [unread channels state](#unread-channels-state-object) object | Collapsed unread channel state |
| mention_frecency? | [mention frecency state](#mention-frecency-state-object) object | Per-guild mention frequency and recency state |
| nagbars? | [nagbar dismissals](#nagbar-dismissals-object) object | Dismissed account and guild notice state |
| dismissed_upsells? | [dismissed upsells](#dismissed-upsells-object) object | Dismissed upsell state |
| guild_nsfw_agreements? | [guild mature content agreements](#guild-mature-content-agreements-object) object | Accepted mature guild, category, and channel notices |
| whats_new? | [what's new state](#whats-new-state-object) object | Last dismissed update entry |
| privacy? | [privacy preferences](#privacy-preferences-object) object | Stream preview, activity, and attachment privacy preferences |
| local_spam_overrides? | [local user spam overrides](#local-user-spam-overrides-object) object | Local user spam classification overrides |
| sanitize_urls<sup>2</sup> | bool | Whether tracking parameters are stripped from the URLs in an outgoing message |
| sound? | [sound settings](#sound-settings-object) object | Sound enablement, volume, and per-sound overrides |
| spellcheck? | [spellcheck settings](#spellcheck-settings-object) object | Spellcheck language, dictionary, and engine preferences |
| search_engines? | [search engine settings](#search-engine-settings-object) object | Text, reverse image, and translation provider choices |
| permission_layout? | [permission layout settings](#permission-layout-settings-object) object | Permission editor layout preferences |
| guild_member_layout? | [guild member layout settings](#guild-member-layout-settings-object) object | Guild member view layout preference |
| guild_folders?<sup>3</sup> | [guild folder expanded state](#guild-folder-expanded-state-object) object | Expanded guild folder identifiers |
| hidden_guild_buttons? | [hidden guild list buttons](#hidden-guild-list-buttons-object) object | Hidden guild list button state |
| keyboard_mode_intro? | [keyboard mode intro state](#keyboard-mode-intro-state-object) object | Keyboard mode introduction state |
| input_monitoring? | [input monitoring prompts state](#input-monitoring-prompts-state-object) object | Input monitoring prompt state |
| voice_prompts? | [voice prompts state](#voice-prompts-state-object) object | Suppressed voice confirmation prompt state |
| sudo_prompt? | [sudo prompt state](#sudo-prompt-state-object) object | Last used sudo verification method |
| keybinds? | [keybind settings](#keybind-settings-object) object | Custom keybinds and transmit mode preferences |
| chat_input? | [chat input settings](#chat-input-settings-object) object | Chat composer behaviour preferences |
| save_camera_uploads_to_device?<sup>4</sup> | bool | Whether a camera upload is also written to the device |

<sup>1</sup> The entries live inside the snapshot, and the account [memes](/http-api/memes/) collection holds none of them

<sup>2</sup> The only bare scalar on the root message, so it has no presence and reads as false until a client stores true. It applies to the content a client is about to send, and the first-party client leaves the URLs inside inline code and code blocks unchanged

<sup>3</sup> Holds only which folders are expanded. The folder layout itself is `guild_folders` on the [user settings object](/http-api/users/#user-settings-object)

<sup>4</sup> The only optional scalar on the root message, so it tracks presence. No live surface reads or writes it

## Accessibility settings object

The `accessibility` field has display, motion, message, media, voice, and interaction presentation preferences. Almost every field is optional, so distinguish an absent field from a stored zero value before applying a default.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| saturation_factor?<sup>1</sup> | double | Colour saturation multiplier |
| always_underline_links | bool | Whether links are always underlined |
| enable_text_selection? | bool | Whether message text can be selected |
| show_message_send_button? | bool | Whether the message composer shows a send button |
| show_textarea_focus_ring? | bool | Whether the message composer shows a focus ring |
| hide_keyboard_hints | bool | Whether keyboard shortcut hints are hidden |
| escape_exits_keyboard_mode? | bool | Whether Escape exits keyboard navigation mode |
| sync_reduced_motion_with_system? | bool | Whether reduced motion follows the operating system preference |
| reduced_motion_override?<sup>2</sup> | bool | Explicit reduced motion preference |
| message_group_spacing?<sup>3</sup> | double | Spacing between message groups |
| message_gutter?<sup>3</sup> | double | Horizontal message gutter size |
| font_size?<sup>3</sup> | double | Message font size |
| show_user_avatars_in_compact_mode? | bool | Whether compact messages show user avatars |
| mobile_sticker_animation_overridden<sup>4</sup> | bool | Whether the mobile sticker animation value applies on a mobile layout |
| mobile_gif_autoplay_overridden<sup>4</sup> | bool | Whether the mobile GIF autoplay value applies on a mobile layout |
| mobile_animate_emoji_overridden<sup>4</sup> | bool | Whether the mobile emoji animation value applies on a mobile layout |
| mobile_sticker_animation_value?<sup>5</sup> | int32 | Mobile [sticker animation setting](/http-api/users/#sticker-animation-settings) |
| mobile_gif_autoplay_value? | bool | Mobile GIF autoplay value |
| mobile_animate_emoji_value? | bool | Mobile emoji animation value |
| auto_send_klipy_gifs | bool | Whether selecting a GIF sends it immediately |
| show_gif_button? | bool | Whether the composer shows the GIF button |
| show_memes_button? | bool | Whether the composer shows the memes button |
| show_stickers_button? | bool | Whether the composer shows the sticker button |
| show_emoji_button? | bool | Whether the composer shows the emoji button |
| show_media_favorite_button? | bool | Whether media actions show the favourite button |
| show_media_download_button? | bool | Whether media actions show the download button |
| show_media_delete_button? | bool | Whether media actions show the delete button |
| show_suppress_embeds_button? | bool | Whether message actions show the suppress embeds button |
| show_gif_indicator? | bool | Whether GIF media shows a GIF indicator |
| show_attachment_expiry_indicator? | bool | Whether attachments show their expiry indicator |
| use_browser_locale_for_time_format? | bool | Whether time formatting uses the browser locale |
| channel_typing_indicator_mode | int32 | [Channel typing indicator mode](#channel-typing-indicator-modes) |
| show_selected_channel_typing_indicator? | bool | Whether the typing indicator is shown for the selected channel |
| show_message_action_bar? | bool | Whether messages show the action bar |
| show_message_action_bar_quick_reactions? | bool | Whether the message action bar shows quick reactions |
| show_message_action_bar_shift_expand? | bool | Whether holding Shift expands the message action bar |
| show_message_action_bar_only_more_button? | bool | Whether the message action bar shows only the more button |
| show_default_emojis_in_autocomplete? | bool | Whether autocomplete includes standard emoji |
| show_custom_emojis_in_autocomplete? | bool | Whether autocomplete includes custom emoji |
| show_stickers_in_autocomplete? | bool | Whether autocomplete includes stickers |
| show_memes_in_autocomplete? | bool | Whether autocomplete includes memes |
| voice_channel_join_requires_double_click? | bool | Whether joining a voice channel requires a double click |
| custom_theme_css?<sup>6</sup> | string | Custom theme CSS |
| show_favorites? | bool | Whether the favourites section is shown |
| zoom_level?<sup>7</sup> | double | Application zoom level |
| dm_message_preview_mode | int32 | [Direct message preview mode](#direct-message-preview-modes) |
| enable_tts_command? | bool | Whether the text-to-speech command is enabled |
| tts_rate?<sup>8</sup> | double | Text-to-speech playback rate |
| show_faded_unread_on_muted_channels? | bool | Whether muted channels retain a faded unread indicator |
| show_context_menu_shortcuts? | bool | Whether context menus show keyboard shortcuts |
| confirm_before_starting_calls? | bool | Whether starting a call requires confirmation |
| hdr_display_mode | int32 | [HDR display mode](#hdr-display-modes) |
| preserve_edit_draft? | bool | Whether cancelling a message edit preserves its draft |
| stay_interactive_when_unfocused? | bool | Whether animation and interaction remain active while the application is unfocused |
| confirm_before_joining_voice_channels? | bool | Whether joining a voice channel requires confirmation |
| screen_reader_announce_new_messages? | bool | Whether a screen reader announces new messages |
| first_click_pass_through_when_unfocused? | bool | Whether the first click also activates its control when the application is unfocused |
| compact_message_group_spacing?<sup>3</sup> | double | Spacing between compact message groups |
| scroll_to_bottom_on_message_send? | bool | Whether sending a message scrolls the channel to the bottom |
| dim_strikethrough_text? | bool | Whether strikethrough text is visually dimmed |
| sequential_file_send? | bool | Whether multiple selected files are sent one message at a time |
| mobile_splash_zoom_animation? | bool | Whether the mobile splash screen plays its zoom animation |

<sup>1</sup> A multiplier applied to interface colour, where 1 leaves colour unchanged

<sup>2</sup> Read only while `sync_reduced_motion_with_system` is false

<sup>3</sup> A length in CSS pixels. The first-party client defaults to 16 for `message_group_spacing`, `message_gutter`, and `font_size`, and to 0 for `compact_message_group_spacing`

<sup>4</sup> The paired `mobile_*_value` field is read only while this flag is true, and only on a mobile layout. While the flag is false the first-party client falls back to the account-level setting for emoji animation, and to a fixed mobile default for GIF autoplay and sticker animation

<sup>5</sup> The same enumeration as `animate_stickers` on the [user settings object](/http-api/users/#user-settings-object). This field is the mobile-local replacement

<sup>6</sup> The complete CSS of the account's synced custom theme, stored inline. A client that has opted out of syncing its theme applies a local one instead and re-emits this value unchanged, so it does not clobber the devices that do sync

<sup>7</sup> A multiplier, where 1 is unscaled. The first-party client keeps its zoom level in browser storage and neither reads nor writes this field

<sup>8</sup> A multiplier, where 1 is the unmodified speaking rate

Field numbers 42 and 43 are reserved, together with the names `attachment_media_dimension_size` and `embed_media_dimension_size`. Neither number can be reused.

### Channel typing indicator modes

| Value | Name | Description |
| --- | --- | --- |
| 0 | CHANNEL_TYPING_INDICATOR_MODE_UNSPECIFIED | No explicit mode is selected |
| 1 | CHANNEL_TYPING_INDICATOR_MODE_AVATARS | Show typing users as avatars |
| 2 | CHANNEL_TYPING_INDICATOR_MODE_INDICATOR_ONLY | Show a typing indicator without avatars |
| 3 | CHANNEL_TYPING_INDICATOR_MODE_HIDDEN | Hide the typing indicator |

### Direct message preview modes

| Value | Name | Description |
| --- | --- | --- |
| 0 | DM_MESSAGE_PREVIEW_MODE_UNSPECIFIED | No explicit mode is selected |
| 1 | DM_MESSAGE_PREVIEW_MODE_ALL | Show previews for all direct messages |
| 2 | DM_MESSAGE_PREVIEW_MODE_UNREAD_ONLY | Show previews only for unread direct messages |
| 3 | DM_MESSAGE_PREVIEW_MODE_NONE | Hide direct message previews |

### HDR display modes

| Value | Name | Description |
| --- | --- | --- |
| 0 | HDR_DISPLAY_MODE_UNSPECIFIED | No explicit mode is selected |
| 1 | HDR_DISPLAY_MODE_FULL | Display HDR media without constraining its range |
| 2 | HDR_DISPLAY_MODE_STANDARD | Display HDR media using the standard presentation |

## Accessibility overrides object

The `accessibility_overrides` field holds three dirty flags that no live surface reads or writes. The settings they name also appear in [accessibility settings](#accessibility-settings-object) as a `mobile_*_overridden` flag with a matching `mobile_*_value`.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| gif_autoplay_dirty | bool | GIF autoplay dirty flag |
| animate_emoji_dirty | bool | Emoji animation dirty flag |
| animate_stickers_dirty | bool | Sticker animation dirty flag |

## Textual preview settings object

The `textual_preview` field controls line wrapping in the inline preview of a textual attachment.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| wrap_text | bool | Whether long lines wrap in a textual preview |

## Usage stat object

The usage stat object records picker usage for one expression or one meme. It is the map value of the `usage` field in every picker state group, so the emoji, sticker, and meme pickers all rank their entries from the same shape.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| count | uint32 | Number of recorded uses |
| last_used_ms<sup>1</sup> | int64 | Unix timestamp in milliseconds of the most recent use |

<sup>1</sup> Milliseconds since 1970-01-01T00:00:00Z

## Emoji picker state object

The `emoji_picker` field records emoji usage, favourites, and collapsed picker categories.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| usage<sup>1</sup> <sup>2</sup> | map[string, [usage stat](#usage-stat-object) object] | Usage keyed by emoji usage key |
| favorite_emoji_ids<sup>1</sup> | array[string] | Favourite emoji usage keys in display order |
| collapsed_category_ids | array[string] | Collapsed emoji category identifiers |

<sup>1</sup> The first-party client writes `unicode:<unique name>` for a standard emoji and `custom:<guild id>:<emoji id>` for a custom emoji

<sup>2</sup> The first-party client drops a key matching neither form when it reads the snapshot, and applies no such filter to `favorite_emoji_ids`

## Sticker picker state object

The `sticker_picker` field records sticker usage, favourites, and collapsed picker categories.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| usage | map[string, [usage stat](#usage-stat-object) object] | Usage keyed by sticker identifier |
| favorite_sticker_ids | array[string] | Favourite sticker identifiers in display order |
| collapsed_category_ids | array[string] | Collapsed sticker category identifiers |

## Memes picker state object

The `memes_picker` field records meme usage, favourites, and collapsed picker categories. The memes themselves belong to the account [memes](/http-api/memes/) collection and are not stored here.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| usage | map[string, [usage stat](#usage-stat-object) object] | Usage keyed by meme identifier |
| favorite_meme_ids | array[string] | Favourite meme identifiers in display order |
| collapsed_category_ids | array[string] | Collapsed meme category identifiers |

## Emoji state object

The `emoji` field stores the selected emoji skin tone.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| skin_tone<sup>1</sup> | string | Selected Unicode skin tone modifier |

<sup>1</sup> One of the five Unicode skin tone modifiers, U+1F3FB through U+1F3FF. The empty string selects the default tone

## Emoji and sticker layout settings object

The `emoji_sticker_layout` field controls how the emoji and sticker pickers lay their contents out.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| emoji_layout | int32 | [Emoji picker layout](#emoji-picker-layouts) |
| sticker_view_mode | int32 | [Sticker picker view mode](#sticker-picker-view-modes) |

### Emoji picker layouts

| Value | Name | Description |
| --- | --- | --- |
| 0 | EMOJI_PICKER_LAYOUT_UNSPECIFIED | No explicit layout is selected |
| 1 | EMOJI_PICKER_LAYOUT_LIST | Display emoji in a list layout |
| 2 | EMOJI_PICKER_LAYOUT_GRID | Display emoji in a grid layout |

### Sticker picker view modes

| Value | Name | Description |
| --- | --- | --- |
| 0 | STICKER_PICKER_VIEW_MODE_UNSPECIFIED | No explicit view mode is selected |
| 1 | STICKER_PICKER_VIEW_MODE_COZY | Display stickers with the roomy layout |
| 2 | STICKER_PICKER_VIEW_MODE_COMPACT | Display stickers with the compact layout |

## Favourite GIF settings object

The `favorite_gifs` field stores the GIFs the account has favourited, together with the metadata needed to render one without another lookup. Favouriting a GIF writes an entry here or writes a meme to the account [memes](/http-api/memes/) collection, never both, and `save_as_saved_media` selects which.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| entries<sup>1</sup> | array[[favourite GIF entry](#favourite-gif-entry-object) object] | Favourite GIFs in display order |
| save_as_saved_media<sup>2</sup> | bool | Whether favouriting a GIF saves a meme to the account memes collection |
| seen_first_time_prompt | bool | Whether the first-time favourite prompt has been shown |

<sup>1</sup> The first-party client treats `url` as the identity of an entry and refuses to append a second entry with a `url` already present

<sup>2</sup> While this is true the first-party client leaves `entries` untouched and favourites through [Save meme from message](/http-api/memes/#save-meme-from-message) instead

## Favourite GIF entry object

One entry describes one favourited GIF. Its fields mirror the [resolved GIF entry object](/http-api/memes/#resolved-gif-entry-object) returned by [Resolve GIF URLs](/http-api/memes/#resolve-gif-urls), so a client can store a resolved entry without reshaping it.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| url | string | Source URL of the GIF |
| proxy_url | string | [Media Proxy](/media-proxy/overview/) URL for the preview format |
| width<sup>1</sup> | uint32 | Width of the preview format in pixels |
| height<sup>1</sup> | uint32 | Height of the preview format in pixels |
| media | map[string, [favourite GIF media format](#favourite-gif-media-format-object) object] | Formats keyed by [GIF media format name](/http-api/memes/#gif-media-format-names) |
| content_type<sup>2</sup> | string | Media type of the media addressed by `proxy_url` |
| placeholder<sup>3</sup> | string | Compact thumbhash placeholder |

<sup>1</sup> Zero when the dimensions could not be determined

<sup>2</sup> The empty string means the type is unknown, which a client reads as `image/gif`

<sup>3</sup> The empty string means the Media Proxy emitted none

## Favourite GIF media format object

One descriptor addresses one encoding of one favourited GIF. It mirrors the [GIF media format object](/http-api/memes/#gif-media-format-object).

### Structure

| Field | Type | Description |
| --- | --- | --- |
| src | string | Direct media URL of this format |
| proxy_src | string | [Media Proxy](/media-proxy/overview/) URL of this format |
| width | uint32 | Width of this format in pixels |
| height | uint32 | Height of this format in pixels |

## Favourites state object

The `favorites` field stores favourite channels, the categories they are grouped into, and the presentation state of that grouping. These categories are private to the favourites view.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| channels | array[[favourite channel](#favourite-channel-object) object] | Favourite channels |
| categories | array[[favourite category](#favourite-category-object) object] | Favourite categories |
| collapsed_category_ids | array[string] | Collapsed favourite category identifiers |
| hide_muted_channels | bool | Whether muted channels are hidden from favourites |
| muted | bool | Whether the favourites collection is muted |

## Favourite channel object

One entry marks one channel as a favourite. Channel and guild identifiers here are strings holding the decimal form of a [snowflake](/snowflakes/). Nothing removes an entry when the account loses access to the channel, so an entry can reference a channel the account can no longer resolve.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| channel_id | string | Channel ID |
| guild_id | string | Guild ID |
| parent_id?<sup>1</sup> | string | Favourite category the channel is grouped under |
| position<sup>2</sup> | int32 | Position across the whole favourites list |
| nickname? | string | User-assigned channel nickname |

<sup>1</sup> The `id` of a [favourite category](#favourite-category-object), not a guild category channel. An absent value groups the channel outside every category

<sup>2</sup> A single ordering over every favourite channel. The first-party client renumbers the whole list from 0 whenever an entry is removed or moved, and appends a new entry at the current entry count without renumbering

## Favourite category object

One entry names one grouping in the favourites view. The client chooses the identifier, so it is not a [snowflake](/snowflakes/) and Fluxer never resolves it.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| id | string | Category identifier |
| name | string | Category name |
| position<sup>1</sup> | int32 | Category position |

<sup>1</sup> Renumbered from 0 across every category whenever one is removed or moved

## Recent mentions settings object

The `recent_mentions` field controls which mentions appear in the recent mentions view. The three filters combine, and `include_guilds` selects by channel.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| include_everyone?<sup>1</sup> | bool | Whether to include `@everyone` mentions |
| include_roles?<sup>1</sup> | bool | Whether to include role mentions |
| include_guilds?<sup>1</sup> <sup>2</sup> | bool | Whether to include mentions in a guild channel |

<sup>1</sup> The first-party client defaults every filter to true and omits the field while it holds that default, so a client that reads an absent value as the Protobuf zero filters out mentions the account expects to see

<sup>2</sup> False keeps direct message mentions and drops every mention whose channel belongs to a guild

## Sidebar preferences object

The `sidebar` field stores direct message sidebar presentation state.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| inline_dms_collapsed | bool | Whether the inline direct message section is collapsed |
| show_collapsed_unread_dms_badge?<sup>1</sup> | bool | Whether the collapsed section shows an unread direct message badge |
| show_incoming_friend_request_badge?<sup>1</sup> | bool | Whether the sidebar shows an incoming friend request badge |

<sup>1</sup> The first-party client defaults it to true and omits the field while it holds that default, so a client that reads an absent value as the Protobuf zero hides a badge the account expects to see

## Member list state object

The `member_list` field stores whether the member list panel is open.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| members_open? | bool | Whether the member list is open |

## Unread channels state object

The `unread_channels` field stores which channels are collapsed in the unread view.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| collapsed_channel_ids | array[string] | Collapsed channel IDs in decimal form |

## Mention frecency state object

The `mention_frecency` field records how often and how recently the account mentioned each user, one record set per guild, so a client can rank mention autocomplete without a server call. It nests two messages, `MentionFrecencyState.Scope` and `MentionFrecencyState.Entry`.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| scopes | array[[mention frecency scope](#mention-frecency-scope-object) object] | Per-guild mention records |

## Mention frecency scope object

One scope holds every mention record for one guild. It is declared as `MentionFrecencyState.Scope`.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| guild_id | string | Guild ID in decimal form |
| entries | array[[mention frecency entry](#mention-frecency-entry-object) object] | User mention records for the guild |

## Mention frecency entry object

One entry counts mentions of one user inside one scope. It is declared as `MentionFrecencyState.Entry`.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| user_id | string | Mentioned user ID in decimal form |
| count | uint32 | Number of recorded mentions |
| last_at_ms<sup>1</sup> | int64 | Unix timestamp in milliseconds of the most recent mention |

<sup>1</sup> Milliseconds since 1970-01-01T00:00:00Z

## Nagbar dismissals object

The `nagbars` field stores dismissed account-wide notices as bools and dismissed per-target notices as maps. Field number 13 is not assigned.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| ios_install | bool | Whether the iOS installation notice is dismissed |
| pwa_install | bool | Whether the progressive web application installation notice is dismissed |
| push_notification | bool | Whether the push notification notice is dismissed |
| desktop_notification | bool | Whether the desktop notification notice is dismissed |
| premium_grace_period | bool | Whether the premium grace period notice is dismissed |
| premium_expired | bool | Whether the premium expiry notice is dismissed |
| premium_onboarding | bool | Whether the premium onboarding notice is dismissed |
| premium_trial_expiring | bool | Whether the premium trial expiry notice is dismissed |
| gift_inventory | bool | Whether the gift inventory notice is dismissed |
| desktop_download | bool | Whether the desktop download notice is dismissed |
| guild_membership_cta | bool | Whether the guild membership call to action notice is dismissed |
| visionary_mfa<sup>1</sup> | bool | Whether the lifetime premium MFA notice is dismissed |
| legacy_phone_unlink | bool | Whether the legacy phone unlink notice is dismissed |
| pending_bulk_deletion | map[string, bool] | Dismissal state keyed by pending bulk deletion identifier |
| invites_disabled<sup>2</sup> | map[string, bool] | Dismissal state keyed by guild ID for disabled-invite notices |
| guild_mfa_requirement<sup>2</sup> | map[string, bool] | Dismissal state keyed by guild ID for MFA requirement notices |

<sup>1</sup> The notice this dismisses is shown only to an account holding lifetime premium with no MFA authenticator enrolled

<sup>2</sup> Keyed by the decimal form of a guild [snowflake](/snowflakes/). A missing key reads as not dismissed

## Dismissed upsells object

The `dismissed_upsells` field stores dismissed upsells.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| picker_premium | bool | Whether the premium picker upsell is dismissed |

## Guild mature content agreements object

The `guild_nsfw_agreements` field stores mature content acknowledgements. Acceptance is recorded per target, so accepting for a guild does not record acceptance for its categories or channels.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| agreed_channel_ids | array[string] | Channel IDs whose mature content notice was accepted |
| agreed_guild_ids | array[string] | Guild IDs whose mature content notice was accepted |
| agreed_category_ids | array[string] | Category IDs whose mature content notice was accepted |

## What's new state object

The `whats_new` field stores the most recently dismissed update entry.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| last_dismissed_entry_id? | string | Most recently dismissed update entry ID |

## Privacy preferences object

The `privacy` field stores client privacy behaviour. Fluxer reads none of it, and none of it changes the privacy fields of the [user settings object](/http-api/users/#user-settings-object), which govern what Fluxer itself discloses about the account.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| disable_stream_previews<sup>1</sup> | bool | Whether stream previews are disabled |
| show_active_now? | bool | Whether the active now view is shown |
| preupload_message_attachments? | bool | Whether message attachments are uploaded before the message is sent |

<sup>1</sup> The only field in this group whose effect reaches past the account's own view. While it is true the first-party client uploads no stream preview frame, so no other viewer receives one

## Local user spam overrides object

The `local_spam_overrides` field stores client-local classifications that override the [SPAMMER public user flag](/http-api/users/#public-user-flags) in presentation only. Fluxer neither reads these lists nor changes any flag because of them.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| spammer_user_ids | array[string] | User IDs presented locally as spammers |
| not_spammer_user_ids<sup>1</sup> | array[string] | User IDs presented locally as not being spammers |

<sup>1</sup> This list wins over `spammer_user_ids` and over the `SPAMMER` flag, so a user ID present in both lists is presented as not a spammer

## Sound settings object

The `sound` field controls sound playback, master volume, and per-sound overrides.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| all_sounds_disabled | bool | Whether every application sound is disabled |
| master_volume?<sup>1</sup> | double | Master sound volume |
| disabled_sounds | map[string, bool] | Disabled state keyed by [sound identifier](#sound-identifiers) |
| sound_overrides<sup>2</sup> | map[string, double] | Volume override keyed by [sound identifier](#sound-identifiers) |

<sup>1</sup> A percentage. The first-party client clamps it to the range 0 through 200, treats 100 as the default, and omits the field entirely while it holds the default

<sup>2</sup> A percentage on the same scale, and the complete volume for that one sound

### Sound identifiers

Both maps are keyed by an arbitrary string, and Fluxer stores and returns any key unchanged. The first-party client uses the identifiers below, and a key outside this set has no defined playback effect.

| Value | Description |
| --- | --- |
| deaf | Deafening sound |
| undeaf | Undeafening sound |
| mute | Muting sound |
| unmute | Unmuting sound |
| message | Message notification sound |
| direct-message | Direct message notification sound |
| same-channel-message | Notification sound for the channel already in view |
| incoming-ring | Incoming call sound |
| user-join | Voice participant join sound |
| user-leave | Voice participant leave sound |
| user-move | Voice participant move sound |
| viewer-join | Stream viewer join sound |
| viewer-leave | Stream viewer leave sound |
| voice-disconnect | Voice disconnection sound |
| camera-on | Camera enabled sound |
| camera-off | Camera disabled sound |
| screen-share-start | Screen share start sound |
| screen-share-stop | Screen share stop sound |

## Spellcheck settings object

The `spellcheck` field controls spellchecking, the selected languages, and the personal dictionary.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| enabled? | bool | Whether spellchecking is enabled |
| languages<sup>1</sup> | array[string] | Selected spellcheck languages |
| personal_dictionary<sup>1</sup> | array[string] | Words in the personal dictionary |
| auto_detect? | bool | Whether spellcheck languages are detected automatically |
| engine?<sup>2</sup> | string | Selected spellcheck engine identifier |

<sup>1</sup> The first-party client drops an empty entry and a duplicate entry when it reads the snapshot

<sup>2</sup> The first-party client recognises `auto`, `system`, and `hunspell`, and ignores any other value

## Search engine settings object

The `search_engines` field stores the selected external providers. Every identifier names a provider the client defines, and Fluxer neither validates nor resolves one.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| text_search_engine_id? | string | Text search engine identifier |
| reverse_image_search_engine_id? | string | Reverse image search engine identifier |
| translation_provider_id? | string | Translation provider identifier |

## Permission layout settings object

The `permission_layout` field controls the layout of the [permission](/http-api/permissions/) editor.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| layout | int32 | [Permission layout mode](#permission-layout-modes) |
| grid | int32 | [Permission grid mode](#permission-grid-modes) |

### Permission layout modes

| Value | Name | Description |
| --- | --- | --- |
| 0 | PERMISSION_LAYOUT_MODE_UNSPECIFIED | No explicit layout is selected |
| 1 | PERMISSION_LAYOUT_MODE_COMFY | Use the roomy permission layout |
| 2 | PERMISSION_LAYOUT_MODE_DENSE | Use the dense permission layout |

### Permission grid modes

| Value | Name | Description |
| --- | --- | --- |
| 0 | PERMISSION_GRID_MODE_UNSPECIFIED | No explicit grid mode is selected |
| 1 | PERMISSION_GRID_MODE_SINGLE | Show one permission category at a time |
| 2 | PERMISSION_GRID_MODE_GRID | Show permission categories in a grid |

## Guild member layout settings object

The `guild_member_layout` field controls how the guild member view presents its members.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| mode | int32 | [Guild member view mode](#guild-member-view-modes) |

### Guild member view modes

| Value | Name | Description |
| --- | --- | --- |
| 0 | GUILD_MEMBER_VIEW_MODE_UNSPECIFIED | No explicit view mode is selected |
| 1 | GUILD_MEMBER_VIEW_MODE_TABLE | Display guild members in a table |
| 2 | GUILD_MEMBER_VIEW_MODE_GRID | Display guild members in a grid |

## Guild folder expanded state object

The `guild_folders` field stores which guild folders are expanded in the sidebar. The folders themselves are `guild_folders` on the [user settings object](/http-api/users/#user-settings-object).

### Structure

| Field | Type | Description |
| --- | --- | --- |
| expanded_folder_ids<sup>1</sup> | array[fixed64] | Expanded guild folder identifiers |

<sup>1</sup> Each value is the `id` of a [guild folder object](/http-api/users/#guild-folder-object). An identifier for a folder the layout no longer holds stays in the array and has no effect

## Hidden guild list buttons object

The `hidden_guild_buttons` field stores which guild list controls are hidden.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| download_button | bool | Whether the download button is hidden |
| help_button | bool | Whether the help button is hidden |

## Keyboard mode intro state object

The `keyboard_mode_intro` field stores whether the keyboard navigation introduction has been seen.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| seen | bool | Whether the keyboard navigation introduction was seen |

## Input monitoring prompts state object

The `input_monitoring` field stores whether the input monitoring prompt has been seen. No live surface reads or writes it.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| seen_cta | bool | Whether the input monitoring call to action was seen |

## Voice prompts state object

The `voice_prompts` field stores which voice confirmation prompts the account has suppressed.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| skip_hide_own_camera_confirm | bool | Whether hiding the caller's own camera skips confirmation |
| skip_hide_own_screenshare_confirm | bool | Whether hiding the caller's own screen share skips confirmation |

## Sudo prompt state object

The `sudo_prompt` field stores the verification method the account used most recently, so a client can preselect it the next time [sudo mode](/http-api/users/mfa/#sudo-mode) is required.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| last_used_mfa_method?<sup>1</sup> | int32 | [MFA method](#mfa-methods) most recently used for sudo verification |

<sup>1</sup> The two assigned values correspond to `totp` and `webauthn` in `mfa_method` of the [sudo verification object](/http-api/users/mfa/#sudo-verification-object)

### MFA methods

| Value | Name | Description |
| --- | --- | --- |
| 0 | MFA_METHOD_UNSPECIFIED | No method is recorded |
| 1 | MFA_METHOD_TOTP | Time-based one-time password |
| 3 | MFA_METHOD_WEBAUTHN | WebAuthn credential |

The value 2 is not assigned.

## Keybind settings object

The `keybinds` field stores custom keybinds and voice transmit behaviour.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| custom_keybinds | array[[custom keybind](#custom-keybind-object) object] | Custom keybinds |
| transmit_mode<sup>1</sup> | string | Voice transmit mode |
| push_to_talk_release_delay_ms?<sup>2</sup> | uint32 | Push-to-talk release delay in milliseconds |

<sup>1</sup> The first-party client recognises `voice_activity` and `voice_push_to_talk`, and falls back to `voice_activity` for the empty string and for any other value

<sup>2</sup> Read only while `transmit_mode` is `voice_push_to_talk`. The first-party client clamps it to the range 20 through 2000, treats 20 as the default, and omits the field entirely while it holds the default

## Custom keybind object

One entry binds one input combination to one action. The entry has its own `enabled` flag and its combination has another. The first-party client gates a custom keybind on the entry's flag alone, and reads `enabled` on a combination only for a built-in default binding.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| id<sup>1</sup> | string | Keybind identifier |
| action?<sup>2</sup> | string | Action performed by the keybind |
| combo? | [keybind combo](#keybind-combo-object) object | Input combination |
| enabled | bool | Whether the keybind is enabled |

<sup>1</sup> Chosen by the client. The first-party client generates a replacement when it reads an entry whose `id` is the empty string

<sup>2</sup> An action name the client defines. The first-party client keeps an entry whose action it does not recognise and clears `action`

## Keybind combo object

One combination describes the key, modifiers, and buttons that trigger a keybind. The modifier flags are independent of each other, and `ctrl_or_meta` is a modifier of its own.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| key | string | Key value |
| code? | string | Physical key code |
| ctrl_or_meta<sup>1</sup> | bool | Whether Control or Meta is required according to the platform |
| ctrl | bool | Whether Control is required |
| alt | bool | Whether Alt is required |
| shift | bool | Whether Shift is required |
| meta | bool | Whether Meta is required |
| global? | bool | Whether the combination is registered globally |
| enabled?<sup>2</sup> | bool | Whether the combination is active |
| modifier_only | bool | Whether the combination contains only modifier keys |
| both_sides | bool | Whether either side of a paired modifier is accepted |
| mouse_button? | uint32 | Mouse button number |
| gamepad_button? | uint32 | Gamepad button number |

<sup>1</sup> Resolves to Meta on macOS and to Control on every other platform, so one stored combination expresses the platform-native accelerator

<sup>2</sup> Distinct from `enabled` on the [custom keybind](#custom-keybind-object) that owns the combination, and with the opposite default, because an absent value here reads as active

## Chat input settings object

The `chat_input` field controls message composer behaviour.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| convert_emoticons? | bool | Whether emoticons are converted to emoji |
