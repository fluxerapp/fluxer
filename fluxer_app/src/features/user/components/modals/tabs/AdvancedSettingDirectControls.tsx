// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	MiddleClickAutoscrollControl,
	SmoothScrollingControl,
	TextSelectionControl,
	VideoSeekThumbnailsControl,
} from '@app/features/user/components/modals/tabs/advanced_settings_tab/AdvancedAccessibilityControls';
import {
	FavoritesControl,
	HideKeyboardHintsControl,
	KeepNekoStillControl,
	ShowNekoControl,
	VoiceChannelJoinBehaviorControl,
} from '@app/features/user/components/modals/tabs/advanced_settings_tab/AdvancedAppearanceControls';
import {
	AutoSendGifsControl,
	ConvertEmoticonsAdvancedControl,
	ExpressionAutocompleteControl,
	HideMutedChannelsByDefaultControl,
	InputButtonsControl,
	MediaButtonsControl,
	MessageActionBarControl,
	PreuploadMessageAttachmentsControl,
	ReverseImageSearchControl,
	SaveGifFavoritesControl,
	ScrollToBottomOnSendControl,
	SearchEnginesControl,
	SequentialFileSendControl,
	SkipMarkAllAsReadControl,
	StripTrackingControl,
	TranslatorsControl,
	TrustAllDomainsControl,
} from '@app/features/user/components/modals/tabs/advanced_settings_tab/AdvancedChatControls';
import {DeveloperModeControl} from '@app/features/user/components/modals/tabs/advanced_settings_tab/AdvancedClientDeveloperControls';
import {
	FirstClickPassThroughControl,
	HardwareAccelerationControl,
	NativeTitleBarControl,
	StayInteractiveUnfocusedControl,
} from '@app/features/user/components/modals/tabs/advanced_settings_tab/AdvancedDesktopControls';
import {UnreadBadgeCustomizationControl} from '@app/features/user/components/modals/tabs/advanced_settings_tab/AdvancedExperimentalControls';
import {
	EmulatedDecodeCodecCapControl,
	OpenH264Control,
	ScreenShareCodecControl,
	ScreenShareEncoderControls,
	ScreenSharePreviewBehaviorControl,
} from '@app/features/user/components/modals/tabs/advanced_settings_tab/AdvancedVideoControls';
import {
	ConnectionVolumeControlsControl,
	NewDeviceAlertsControl,
} from '@app/features/user/components/modals/tabs/advanced_settings_tab/AdvancedVoiceControls';
import type {SearchableSettingItem} from '@app/features/user/components/settings_utils/SettingsSectionRegistry';
import {observer} from 'mobx-react-lite';

export const DIRECT_CONTROL_ITEM_IDS = new Set([
	'accessibility-text-selection',
	'accessibility-video-seek-thumbnails',
	'accessibility-smooth-scrolling',
	'accessibility-middle-click-autoscroll',
	'appearance-show-neko',
	'appearance-keep-neko-still',
	'appearance-hide-keyboard-hints',
	'appearance-voice-channel-join-behavior',
	'appearance-enable-favorites',
	'chat-settings-auto-send-gifs',
	'chat-settings-save-gif-favorites',
	'chat-settings-message-action-bar',
	'chat-settings-media-buttons',
	'chat-settings-strip-tracking',
	'chat-settings-trust-domains',
	'chat-settings-search-engines',
	'chat-settings-translators',
	'chat-settings-reverse-image-search',
	'chat-settings-expression-autocomplete',
	'chat-settings-input-buttons',
	'chat-settings-convert-emoticons',
	'chat-settings-preupload-attachments',
	'chat-settings-sequential-file-send',
	'chat-settings-scroll-to-bottom-on-send',
	'chat-settings-skip-mark-all-as-read-confirmation',
	'chat-settings-hide-muted-channels',
	'voice-video-new-device-alerts',
	'voice-video-connection-volume-controls',
	'voice-video-screen-share-codec',
	'voice-video-emulated-decode-codec-cap',
	'voice-video-openh264-codec',
	'voice-video-screen-share-preview-behavior',
	'voice-video-screen-share-encoder-controls',
	'advanced-unread-badge-customization',
	'client-developer-mode',
	'advanced-stay-interactive-unfocused',
	'first-click-pass-through',
	'advanced-native-title-bar',
	'advanced-hardware-acceleration',
]);

export const FULL_WIDTH_CONTROL_ITEM_IDS = new Set([
	'appearance-voice-channel-join-behavior',
	'voice-video-screen-share-codec',
	'voice-video-screen-share-preview-behavior',
]);

export const COMPACT_SWITCH_CONTROL_ITEM_IDS = new Set([
	'accessibility-text-selection',
	'accessibility-video-seek-thumbnails',
	'accessibility-smooth-scrolling',
	'accessibility-middle-click-autoscroll',
	'appearance-show-neko',
	'appearance-keep-neko-still',
	'appearance-hide-keyboard-hints',
	'appearance-enable-favorites',
	'chat-settings-auto-send-gifs',
	'chat-settings-save-gif-favorites',
	'chat-settings-strip-tracking',
	'chat-settings-trust-domains',
	'chat-settings-convert-emoticons',
	'chat-settings-preupload-attachments',
	'chat-settings-sequential-file-send',
	'chat-settings-scroll-to-bottom-on-send',
	'chat-settings-skip-mark-all-as-read-confirmation',
	'chat-settings-hide-muted-channels',
	'voice-video-new-device-alerts',
	'voice-video-connection-volume-controls',
	'voice-video-openh264-codec',
	'advanced-unread-badge-customization',
	'client-developer-mode',
	'advanced-stay-interactive-unfocused',
	'first-click-pass-through',
	'advanced-native-title-bar',
	'advanced-hardware-acceleration',
]);

export const AdvancedSettingControl = observer(({item}: {item: SearchableSettingItem}) => {
	switch (item.id) {
		case 'accessibility-text-selection':
			return (
				<TextSelectionControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.text-selection-control" />
			);
		case 'accessibility-video-seek-thumbnails':
			return (
				<VideoSeekThumbnailsControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.video-seek-thumbnails-control" />
			);
		case 'accessibility-smooth-scrolling':
			return (
				<SmoothScrollingControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.smooth-scrolling-control" />
			);
		case 'accessibility-middle-click-autoscroll':
			return (
				<MiddleClickAutoscrollControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.middle-click-autoscroll-control" />
			);
		case 'appearance-show-neko':
			return (
				<ShowNekoControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.show-neko-control" />
			);
		case 'appearance-keep-neko-still':
			return (
				<KeepNekoStillControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.keep-neko-still-control" />
			);
		case 'appearance-hide-keyboard-hints':
			return (
				<HideKeyboardHintsControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.hide-keyboard-hints-control" />
			);
		case 'appearance-voice-channel-join-behavior':
			return (
				<VoiceChannelJoinBehaviorControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.voice-channel-join-behavior-control" />
			);
		case 'appearance-enable-favorites':
			return (
				<FavoritesControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.favorites-control" />
			);
		case 'chat-settings-auto-send-gifs':
			return (
				<AutoSendGifsControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.auto-send-gifs-control" />
			);
		case 'chat-settings-save-gif-favorites':
			return (
				<SaveGifFavoritesControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.save-gif-favorites-control" />
			);
		case 'chat-settings-message-action-bar':
			return (
				<MessageActionBarControl
					title={item.label}
					data-flx="user.advanced-setting-direct-controls.advanced-setting-control.message-action-bar-control"
				/>
			);
		case 'chat-settings-media-buttons':
			return (
				<MediaButtonsControl
					title={item.label}
					data-flx="user.advanced-setting-direct-controls.advanced-setting-control.media-buttons-control"
				/>
			);
		case 'chat-settings-strip-tracking':
			return (
				<StripTrackingControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.strip-tracking-control" />
			);
		case 'chat-settings-trust-domains':
			return (
				<TrustAllDomainsControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.trust-all-domains-control" />
			);
		case 'chat-settings-search-engines':
			return (
				<SearchEnginesControl
					title={item.label}
					data-flx="user.advanced-setting-direct-controls.advanced-setting-control.search-engines-control"
				/>
			);
		case 'chat-settings-translators':
			return (
				<TranslatorsControl
					title={item.label}
					data-flx="user.advanced-setting-direct-controls.advanced-setting-control.translators-control"
				/>
			);
		case 'chat-settings-reverse-image-search':
			return (
				<ReverseImageSearchControl
					title={item.label}
					data-flx="user.advanced-setting-direct-controls.advanced-setting-control.reverse-image-search-control"
				/>
			);
		case 'chat-settings-expression-autocomplete':
			return (
				<ExpressionAutocompleteControl
					title={item.label}
					data-flx="user.advanced-setting-direct-controls.advanced-setting-control.expression-autocomplete-control"
				/>
			);
		case 'chat-settings-input-buttons':
			return (
				<InputButtonsControl
					title={item.label}
					data-flx="user.advanced-setting-direct-controls.advanced-setting-control.input-buttons-control"
				/>
			);
		case 'chat-settings-convert-emoticons':
			return (
				<ConvertEmoticonsAdvancedControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.convert-emoticons-advanced-control" />
			);
		case 'chat-settings-preupload-attachments':
			return (
				<PreuploadMessageAttachmentsControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.preupload-message-attachments-control" />
			);
		case 'chat-settings-sequential-file-send':
			return (
				<SequentialFileSendControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.sequential-file-send-control" />
			);
		case 'chat-settings-scroll-to-bottom-on-send':
			return (
				<ScrollToBottomOnSendControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.scroll-to-bottom-on-send-control" />
			);
		case 'chat-settings-skip-mark-all-as-read-confirmation':
			return (
				<SkipMarkAllAsReadControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.skip-mark-all-as-read-control" />
			);
		case 'chat-settings-hide-muted-channels':
			return (
				<HideMutedChannelsByDefaultControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.hide-muted-channels-by-default-control" />
			);
		case 'voice-video-new-device-alerts':
			return (
				<NewDeviceAlertsControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.new-device-alerts-control" />
			);
		case 'voice-video-connection-volume-controls':
			return (
				<ConnectionVolumeControlsControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.connection-volume-controls-control" />
			);
		case 'voice-video-screen-share-codec':
			return (
				<ScreenShareCodecControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.screen-share-codec-control" />
			);
		case 'voice-video-emulated-decode-codec-cap':
			return (
				<EmulatedDecodeCodecCapControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.emulated-decode-codec-cap-control" />
			);
		case 'voice-video-openh264-codec':
			return (
				<OpenH264Control data-flx="user.advanced-setting-direct-controls.advanced-setting-control.open-h264-control" />
			);
		case 'voice-video-screen-share-preview-behavior':
			return (
				<ScreenSharePreviewBehaviorControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.screen-share-preview-behavior-control" />
			);
		case 'voice-video-screen-share-encoder-controls':
			return (
				<ScreenShareEncoderControls
					title={item.label}
					data-flx="user.advanced-setting-direct-controls.advanced-setting-control.screen-share-encoder-controls"
				/>
			);
		case 'advanced-unread-badge-customization':
			return (
				<UnreadBadgeCustomizationControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.unread-badge-customization-control" />
			);
		case 'client-developer-mode':
			return (
				<DeveloperModeControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.developer-mode-control" />
			);
		case 'advanced-stay-interactive-unfocused':
			return (
				<StayInteractiveUnfocusedControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.stay-interactive-unfocused-control" />
			);
		case 'first-click-pass-through':
			return (
				<FirstClickPassThroughControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.first-click-pass-through-control" />
			);
		case 'advanced-native-title-bar':
			return (
				<NativeTitleBarControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.native-title-bar-control" />
			);
		case 'advanced-hardware-acceleration':
			return (
				<HardwareAccelerationControl data-flx="user.advanced-setting-direct-controls.advanced-setting-control.hardware-acceleration-control" />
			);
		default:
			return null;
	}
});
