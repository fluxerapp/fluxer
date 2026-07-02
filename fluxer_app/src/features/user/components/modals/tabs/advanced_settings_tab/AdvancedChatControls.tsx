// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AccessibilityCommands from '@app/features/accessibility/commands/AccessibilityCommands';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import FavoriteGif from '@app/features/expressions/state/FavoriteGif';
import Guilds from '@app/features/guild/state/Guilds';
import Inbox from '@app/features/inbox/state/Inbox';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {SwitchGroup, SwitchGroupItem} from '@app/features/ui/components/SwitchGroup';
import * as UserGuildSettingsCommands from '@app/features/user/commands/UserGuildSettingsCommands';
import * as UserSettingsCommands from '@app/features/user/commands/UserSettingsCommands';
import type {SearchEngineMode} from '@app/features/user/components/modals/tabs/chat_settings_tab/AddCustomSearchEngineModal';
import {ConvertEmoticonsControl} from '@app/features/user/components/modals/tabs/chat_settings_tab/ChatSettingsTabInputTab';
import {
	StripTrackingParametersControl,
	TrustAllExternalLinksControl,
} from '@app/features/user/components/modals/tabs/chat_settings_tab/ChatSettingsTabLinksTab';
import {
	ReverseImageSearchContent,
	TextSearchEnginesContent,
	TranslatorsContent,
} from '@app/features/user/components/modals/tabs/chat_settings_tab/SearchEnginesTab';
import PrivacyPreferences from '@app/features/user/state/PrivacyPreferences';
import UserSettings from '@app/features/user/state/UserSettings';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {GearIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

const AUTOMATICALLY_SEND_GIFS_WHEN_SELECTED_DESCRIPTOR = msg({
	message: 'Automatically send GIFs when selected',
	comment: 'Short label for an advanced GIF behavior preference.',
});
const SAVE_GIF_FAVORITES_AS_SAVED_MEDIA_DESCRIPTOR = msg({
	message: 'Save GIF favorites as saved media',
	comment: 'Short label for an advanced GIF storage preference.',
});
const SHOW_MESSAGE_ACTION_BAR_DESCRIPTOR = msg({
	message: 'Show message action bar',
	comment: 'Short label for an advanced message action bar preference.',
});
const SHOW_ONLY_MORE_BUTTON_DESCRIPTOR = msg({
	message: 'Show only more button',
	comment: 'Short label for an advanced message action bar preference.',
});
const SHOW_QUICK_REACTIONS_DESCRIPTOR = msg({
	message: 'Show quick reactions',
	comment: 'Short label for an advanced message action bar preference.',
});
const ENABLE_SHIFT_TO_EXPAND_DESCRIPTOR = msg({
	message: 'Enable Shift to expand',
	comment: 'Short label for an advanced message action bar preference.',
});
const SHOW_GIF_INDICATOR_DESCRIPTOR = msg({
	message: 'Show GIF indicator',
	comment: 'Short label for an advanced media button preference.',
});
const SHOW_ATTACHMENT_EXPIRY_INDICATOR_DESCRIPTOR = msg({
	message: 'Show attachment expiry indicator',
	comment: 'Short label for an advanced media button preference.',
});
const SHOW_DELETE_BUTTON_DESCRIPTOR = msg({
	message: 'Show delete button',
	comment: 'Short label for an advanced media button preference.',
});
const SHOW_DOWNLOAD_BUTTON_DESCRIPTOR = msg({
	message: 'Show download button',
	comment: 'Short label for an advanced media button preference.',
});
const SHOW_FAVORITE_BUTTON_DESCRIPTOR = msg({
	message: 'Show favorite button',
	comment: 'Short label for an advanced media button preference.',
});
const SHOW_SUPPRESS_EMBEDS_BUTTON_DESCRIPTOR = msg({
	message: 'Show suppress embeds button',
	comment: 'Short label for an advanced media button preference.',
});
const SHOW_DEFAULT_EMOJIS_IN_EXPRESSION_AUTOCOMPLETE_DESCRIPTOR = msg({
	message: 'Show default emojis in expression autocomplete',
	comment: 'Short label for an advanced message input preference.',
});
const SHOW_CUSTOM_EMOJIS_IN_EXPRESSION_AUTOCOMPLETE_DESCRIPTOR = msg({
	message: 'Show custom emojis in expression autocomplete',
	comment: 'Short label for an advanced message input preference.',
});
const SHOW_STICKERS_IN_EXPRESSION_AUTOCOMPLETE_DESCRIPTOR = msg({
	message: 'Show stickers in expression autocomplete',
	comment: 'Short label for an advanced message input preference.',
});
const SHOW_SAVED_MEDIA_IN_EXPRESSION_AUTOCOMPLETE_DESCRIPTOR = msg({
	message: 'Show saved media in expression autocomplete',
	comment: 'Short label for an advanced message input preference.',
});
const SHOW_GIFS_BUTTON_DESCRIPTOR = msg({
	message: 'Show GIFs button',
	comment: 'Short label for an advanced message input preference.',
});
const SHOW_MEDIA_BUTTON_DESCRIPTOR = msg({
	message: 'Show media button',
	comment: 'Short label for an advanced message input preference.',
});
const UPLOAD_ATTACHMENTS_BEFORE_SENDING_DESCRIPTOR = msg({
	message: 'Upload attachments before sending',
	comment: 'Short label for an advanced message input privacy preference.',
});
const SEND_FILES_IN_SEQUENTIAL_ORDER_DESCRIPTOR = msg({
	message: 'Send file messages in order',
	comment: 'Short label for an advanced message input preference.',
});
const SHOW_STICKERS_BUTTON_DESCRIPTOR = msg({
	message: 'Show stickers button',
	comment: 'Short label for an advanced message input preference.',
});
const SHOW_EMOJI_BUTTON_DESCRIPTOR = msg({
	message: 'Show emoji button',
	comment: 'Short label for an advanced message input preference.',
});
const SHOW_SEND_BUTTON_DESCRIPTOR = msg({
	message: 'Show send button',
	comment: 'Short label for an advanced message input preference.',
});
const SCROLL_TO_BOTTOM_WHEN_SENDING_A_MESSAGE_DESCRIPTOR = msg({
	message: 'Scroll to bottom when sending a message',
	comment: 'Short label for an advanced message input preference.',
});
const SKIP_MARK_ALL_AS_READ_CONFIRMATION_DESCRIPTOR = msg({
	message: 'Skip "Mark all as read" confirmation',
	comment: 'Short label for an advanced inbox preference.',
});
const HIDE_MUTED_CHANNELS_BY_DEFAULT_DESCRIPTOR = msg({
	message: 'Hide muted channels by default',
	comment: 'Short label for an advanced sidebar preference.',
});
const HIDE_MUTED_CHANNELS_BY_DEFAULT_PROMPT_DESCRIPTOR = msg({
	message: 'Hide muted channels by default?',
	comment: 'Confirmation prompt in advanced settings.',
});
const APPLY_TO_ALL_COMMUNITIES_DESCRIPTOR = msg({
	message: 'Apply to all communities',
	comment: 'Short confirmation button label in advanced settings.',
});
const NEW_COMMUNITIES_ONLY_DESCRIPTOR = msg({
	message: 'New communities only',
	comment: 'Short confirmation button label in advanced settings.',
});
const STOP_HIDING_MUTED_CHANNELS_BY_DEFAULT_DESCRIPTOR = msg({
	message: 'Stop hiding muted channels by default?',
	comment: 'Confirmation prompt in advanced settings.',
});
const SHOW_IN_ALL_COMMUNITIES_DESCRIPTOR = msg({
	message: 'Show in all communities',
	comment: 'Short confirmation button label in advanced settings.',
});
const CONFIGURE_DESCRIPTOR = msg({
	message: 'Configure',
	comment: 'Button label that opens a dedicated advanced settings modal.',
});

export const AutoSendGifsControl = observer(() => {
	const {i18n} = useLingui();
	return (
		<Switch
			ariaLabel={i18n._(AUTOMATICALLY_SEND_GIFS_WHEN_SELECTED_DESCRIPTOR)}
			value={Accessibility.autoSendKlipyGifs}
			onChange={(value) => AccessibilityCommands.update({autoSendKlipyGifs: value})}
			compact
			data-flx="user.advanced-settings-tab.switch.auto-send-gifs"
		/>
	);
});

export const SaveGifFavoritesControl = observer(() => {
	const {i18n} = useLingui();
	return (
		<Switch
			ariaLabel={i18n._(SAVE_GIF_FAVORITES_AS_SAVED_MEDIA_DESCRIPTOR)}
			value={FavoriteGif.saveGifFavoritesAsSavedMedia}
			onChange={(value) => FavoriteGif.setSaveGifFavoritesAsSavedMedia(value)}
			compact
			data-flx="user.advanced-settings-tab.switch.save-gif-favorites"
		/>
	);
});

const MessageActionBarContent = observer(() => {
	const {i18n} = useLingui();
	return (
		<SwitchGroup data-flx="user.advanced-settings-tab.switch-group.message-action-bar">
			<SwitchGroupItem
				label={i18n._(SHOW_MESSAGE_ACTION_BAR_DESCRIPTOR)}
				value={Accessibility.showMessageActionBar}
				onChange={(value) => AccessibilityCommands.update({showMessageActionBar: value})}
				data-flx="user.advanced-settings-tab.switch-group-item.message-action-bar"
			/>
			<SwitchGroupItem
				label={i18n._(SHOW_ONLY_MORE_BUTTON_DESCRIPTOR)}
				value={Accessibility.showMessageActionBarOnlyMoreButton}
				onChange={(value) => AccessibilityCommands.update({showMessageActionBarOnlyMoreButton: value})}
				disabled={!Accessibility.showMessageActionBar}
				data-flx="user.advanced-settings-tab.switch-group-item.message-action-bar-more"
			/>
			<SwitchGroupItem
				label={i18n._(SHOW_QUICK_REACTIONS_DESCRIPTOR)}
				value={Accessibility.showMessageActionBarQuickReactions}
				onChange={(value) => AccessibilityCommands.update({showMessageActionBarQuickReactions: value})}
				disabled={!Accessibility.showMessageActionBar || Accessibility.showMessageActionBarOnlyMoreButton}
				data-flx="user.advanced-settings-tab.switch-group-item.message-action-bar-reactions"
			/>
			<SwitchGroupItem
				label={i18n._(ENABLE_SHIFT_TO_EXPAND_DESCRIPTOR)}
				value={Accessibility.showMessageActionBarShiftExpand}
				onChange={(value) => AccessibilityCommands.update({showMessageActionBarShiftExpand: value})}
				disabled={!Accessibility.showMessageActionBar || Accessibility.showMessageActionBarOnlyMoreButton}
				data-flx="user.advanced-settings-tab.switch-group-item.message-action-bar-shift"
			/>
		</SwitchGroup>
	);
});

const MediaButtonsContent = observer(() => {
	const {i18n} = useLingui();
	return (
		<SwitchGroup data-flx="user.advanced-settings-tab.switch-group.media-buttons">
			<SwitchGroupItem
				label={i18n._(SHOW_GIF_INDICATOR_DESCRIPTOR)}
				value={Accessibility.showGifIndicator}
				onChange={(value) => AccessibilityCommands.update({showGifIndicator: value})}
				data-flx="user.advanced-settings-tab.switch-group-item.gif-indicator"
			/>
			<SwitchGroupItem
				label={i18n._(SHOW_ATTACHMENT_EXPIRY_INDICATOR_DESCRIPTOR)}
				value={Accessibility.showAttachmentExpiryIndicator}
				onChange={(value) => AccessibilityCommands.update({showAttachmentExpiryIndicator: value})}
				data-flx="user.advanced-settings-tab.switch-group-item.attachment-expiry"
			/>
			<SwitchGroupItem
				label={i18n._(SHOW_DELETE_BUTTON_DESCRIPTOR)}
				value={Accessibility.showMediaDeleteButton}
				onChange={(value) => AccessibilityCommands.update({showMediaDeleteButton: value})}
				data-flx="user.advanced-settings-tab.switch-group-item.media-delete"
			/>
			<SwitchGroupItem
				label={i18n._(SHOW_DOWNLOAD_BUTTON_DESCRIPTOR)}
				value={Accessibility.showMediaDownloadButton}
				onChange={(value) => AccessibilityCommands.update({showMediaDownloadButton: value})}
				data-flx="user.advanced-settings-tab.switch-group-item.media-download"
			/>
			<SwitchGroupItem
				label={i18n._(SHOW_FAVORITE_BUTTON_DESCRIPTOR)}
				value={Accessibility.showMediaFavoriteButton}
				onChange={(value) => AccessibilityCommands.update({showMediaFavoriteButton: value})}
				data-flx="user.advanced-settings-tab.switch-group-item.media-favorite"
			/>
			<SwitchGroupItem
				label={i18n._(SHOW_SUPPRESS_EMBEDS_BUTTON_DESCRIPTOR)}
				value={Accessibility.showSuppressEmbedsButton}
				onChange={(value) => AccessibilityCommands.update({showSuppressEmbedsButton: value})}
				data-flx="user.advanced-settings-tab.switch-group-item.suppress-embeds"
			/>
		</SwitchGroup>
	);
});

export const StripTrackingControl = observer(() => (
	<StripTrackingParametersControl compact data-flx="user.advanced-settings-tab.strip-tracking-control" />
));

export const TrustAllDomainsControl = observer(() => (
	<TrustAllExternalLinksControl compact data-flx="user.advanced-settings-tab.trust-all-domains-control" />
));

export const ConvertEmoticonsAdvancedControl = observer(() => (
	<ConvertEmoticonsControl compact data-flx="user.advanced-settings-tab.convert-emoticons-control" />
));

interface SearchProviderSettingsModalProps {
	mode: SearchEngineMode;
	title: string;
}

const SearchProviderSettingsModal = observer(({mode, title}: SearchProviderSettingsModalProps) => {
	const handleClose = useCallback(() => {
		ModalCommands.pop();
	}, []);
	const content =
		mode === 'image' ? (
			<ReverseImageSearchContent data-flx="user.advanced-settings-tab.provider-settings-modal.reverse-image-search-content" />
		) : mode === 'translate' ? (
			<TranslatorsContent data-flx="user.advanced-settings-tab.provider-settings-modal.translators-content" />
		) : (
			<TextSearchEnginesContent data-flx="user.advanced-settings-tab.provider-settings-modal.search-engines-content" />
		);
	return (
		<Modal.Root
			size="medium"
			onClose={handleClose}
			data-flx="user.advanced-settings-tab.provider-settings-modal.modal-root"
		>
			<Modal.Header
				title={title}
				onClose={handleClose}
				data-flx="user.advanced-settings-tab.provider-settings-modal.modal-header"
			/>
			<Modal.Content data-flx="user.advanced-settings-tab.provider-settings-modal.modal-content">
				<Modal.ContentLayout data-flx="user.advanced-settings-tab.provider-settings-modal.modal-content-layout">
					{content}
				</Modal.ContentLayout>
			</Modal.Content>
		</Modal.Root>
	);
});

interface SearchProviderSettingsButtonProps {
	mode: SearchEngineMode;
	title: string;
	dataFlx: string;
}

const SearchProviderSettingsButton = observer(({mode, title, dataFlx}: SearchProviderSettingsButtonProps) => {
	const {i18n} = useLingui();
	const handleOpen = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<SearchProviderSettingsModal
					mode={mode}
					title={title}
					data-flx="user.advanced-settings-tab.advanced-chat-controls.handle-open.search-provider-settings-modal"
				/>
			)),
		);
	}, [mode, title]);
	return (
		<Button
			variant="secondary"
			compact
			leftIcon={
				<GearIcon size={14} weight="bold" data-flx="user.advanced-settings-tab.provider-settings-button.gear-icon" />
			}
			onClick={handleOpen}
			data-flx={dataFlx}
		>
			{i18n._(CONFIGURE_DESCRIPTOR)}
		</Button>
	);
});

interface SearchProviderControlProps {
	title: string;
}

export const SearchEnginesControl = observer(({title}: SearchProviderControlProps) => (
	<SearchProviderSettingsButton
		mode="text"
		title={title}
		dataFlx="user.advanced-settings-tab.search-engines-control.configure-button"
		data-flx="user.advanced-settings-tab.advanced-chat-controls.search-engines-control.search-provider-settings-button"
	/>
));

export const TranslatorsControl = observer(({title}: SearchProviderControlProps) => (
	<SearchProviderSettingsButton
		mode="translate"
		title={title}
		dataFlx="user.advanced-settings-tab.translators-control.configure-button"
		data-flx="user.advanced-settings-tab.advanced-chat-controls.translators-control.search-provider-settings-button"
	/>
));

export const ReverseImageSearchControl = observer(({title}: SearchProviderControlProps) => (
	<SearchProviderSettingsButton
		mode="image"
		title={title}
		dataFlx="user.advanced-settings-tab.reverse-image-search-control.configure-button"
		data-flx="user.advanced-settings-tab.advanced-chat-controls.reverse-image-search-control.search-provider-settings-button"
	/>
));

const ExpressionAutocompleteContent = observer(() => {
	const {i18n} = useLingui();
	return (
		<SwitchGroup data-flx="user.advanced-settings-tab.switch-group.expression-autocomplete">
			<SwitchGroupItem
				label={i18n._(SHOW_DEFAULT_EMOJIS_IN_EXPRESSION_AUTOCOMPLETE_DESCRIPTOR)}
				value={Accessibility.showDefaultEmojisInExpressionAutocomplete}
				onChange={(value) => AccessibilityCommands.update({showDefaultEmojisInExpressionAutocomplete: value})}
				data-flx="user.advanced-settings-tab.switch-group-item.default-emojis"
			/>
			<SwitchGroupItem
				label={i18n._(SHOW_CUSTOM_EMOJIS_IN_EXPRESSION_AUTOCOMPLETE_DESCRIPTOR)}
				value={Accessibility.showCustomEmojisInExpressionAutocomplete}
				onChange={(value) => AccessibilityCommands.update({showCustomEmojisInExpressionAutocomplete: value})}
				data-flx="user.advanced-settings-tab.switch-group-item.custom-emojis"
			/>
			<SwitchGroupItem
				label={i18n._(SHOW_STICKERS_IN_EXPRESSION_AUTOCOMPLETE_DESCRIPTOR)}
				value={Accessibility.showStickersInExpressionAutocomplete}
				onChange={(value) => AccessibilityCommands.update({showStickersInExpressionAutocomplete: value})}
				data-flx="user.advanced-settings-tab.switch-group-item.stickers"
			/>
			<SwitchGroupItem
				label={i18n._(SHOW_SAVED_MEDIA_IN_EXPRESSION_AUTOCOMPLETE_DESCRIPTOR)}
				value={Accessibility.showMemesInExpressionAutocomplete}
				onChange={(value) => AccessibilityCommands.update({showMemesInExpressionAutocomplete: value})}
				data-flx="user.advanced-settings-tab.switch-group-item.saved-media"
			/>
		</SwitchGroup>
	);
});

const InputButtonsContent = observer(() => {
	const {i18n} = useLingui();
	return (
		<SwitchGroup data-flx="user.advanced-settings-tab.switch-group.input-buttons">
			<SwitchGroupItem
				label={i18n._(SHOW_GIFS_BUTTON_DESCRIPTOR)}
				value={Accessibility.showGifButton}
				onChange={(value) => AccessibilityCommands.update({showGifButton: value})}
				data-flx="user.advanced-settings-tab.switch-group-item.gif-button"
			/>
			<SwitchGroupItem
				label={i18n._(SHOW_MEDIA_BUTTON_DESCRIPTOR)}
				value={Accessibility.showMemesButton}
				onChange={(value) => AccessibilityCommands.update({showMemesButton: value})}
				data-flx="user.advanced-settings-tab.switch-group-item.media-button"
			/>
			<SwitchGroupItem
				label={i18n._(SHOW_STICKERS_BUTTON_DESCRIPTOR)}
				value={Accessibility.showStickersButton}
				onChange={(value) => AccessibilityCommands.update({showStickersButton: value})}
				data-flx="user.advanced-settings-tab.switch-group-item.stickers-button"
			/>
			<SwitchGroupItem
				label={i18n._(SHOW_EMOJI_BUTTON_DESCRIPTOR)}
				value={Accessibility.showEmojiButton}
				onChange={(value) => AccessibilityCommands.update({showEmojiButton: value})}
				data-flx="user.advanced-settings-tab.switch-group-item.emoji-button"
			/>
			<SwitchGroupItem
				label={i18n._(SHOW_SEND_BUTTON_DESCRIPTOR)}
				value={Accessibility.showMessageSendButton}
				onChange={(value) => AccessibilityCommands.update({showMessageSendButton: value})}
				data-flx="user.advanced-settings-tab.switch-group-item.send-button"
			/>
		</SwitchGroup>
	);
});

type SwitchGroupSettingsMode = 'message-action-bar' | 'media-buttons' | 'expression-autocomplete' | 'input-buttons';

interface SwitchGroupSettingsModalProps {
	mode: SwitchGroupSettingsMode;
	title: string;
}

const SwitchGroupSettingsModal = observer(({mode, title}: SwitchGroupSettingsModalProps) => {
	const handleClose = useCallback(() => {
		ModalCommands.pop();
	}, []);
	const content =
		mode === 'message-action-bar' ? (
			<MessageActionBarContent data-flx="user.advanced-settings-tab.advanced-chat-controls.switch-group-settings-modal.message-action-bar-content" />
		) : mode === 'media-buttons' ? (
			<MediaButtonsContent data-flx="user.advanced-settings-tab.advanced-chat-controls.switch-group-settings-modal.media-buttons-content" />
		) : mode === 'expression-autocomplete' ? (
			<ExpressionAutocompleteContent data-flx="user.advanced-settings-tab.advanced-chat-controls.switch-group-settings-modal.expression-autocomplete-content" />
		) : (
			<InputButtonsContent data-flx="user.advanced-settings-tab.advanced-chat-controls.switch-group-settings-modal.input-buttons-content" />
		);
	return (
		<Modal.Root
			size="medium"
			onClose={handleClose}
			data-flx="user.advanced-settings-tab.switch-group-settings-modal.modal-root"
		>
			<Modal.Header
				title={title}
				onClose={handleClose}
				data-flx="user.advanced-settings-tab.switch-group-settings-modal.modal-header"
			/>
			<Modal.Content data-flx="user.advanced-settings-tab.switch-group-settings-modal.modal-content">
				<Modal.ContentLayout data-flx="user.advanced-settings-tab.switch-group-settings-modal.modal-content-layout">
					{content}
				</Modal.ContentLayout>
			</Modal.Content>
		</Modal.Root>
	);
});

interface SwitchGroupSettingsButtonProps {
	mode: SwitchGroupSettingsMode;
	title: string;
	dataFlx: string;
}

const SwitchGroupSettingsButton = observer(({mode, title, dataFlx}: SwitchGroupSettingsButtonProps) => {
	const {i18n} = useLingui();
	const handleOpen = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<SwitchGroupSettingsModal
					mode={mode}
					title={title}
					data-flx="user.advanced-settings-tab.advanced-chat-controls.handle-open.switch-group-settings-modal"
				/>
			)),
		);
	}, [mode, title]);
	return (
		<Button
			variant="secondary"
			compact
			leftIcon={
				<GearIcon
					size={14}
					weight="bold"
					data-flx="user.advanced-settings-tab.switch-group-settings-button.gear-icon"
				/>
			}
			onClick={handleOpen}
			data-flx={dataFlx}
		>
			{i18n._(CONFIGURE_DESCRIPTOR)}
		</Button>
	);
});

interface SwitchGroupSettingsControlProps {
	title: string;
}

export const MessageActionBarControl = observer(({title}: SwitchGroupSettingsControlProps) => (
	<SwitchGroupSettingsButton
		mode="message-action-bar"
		title={title}
		dataFlx="user.advanced-settings-tab.message-action-bar-control.configure-button"
		data-flx="user.advanced-settings-tab.advanced-chat-controls.message-action-bar-control.switch-group-settings-button"
	/>
));

export const MediaButtonsControl = observer(({title}: SwitchGroupSettingsControlProps) => (
	<SwitchGroupSettingsButton
		mode="media-buttons"
		title={title}
		dataFlx="user.advanced-settings-tab.media-buttons-control.configure-button"
		data-flx="user.advanced-settings-tab.advanced-chat-controls.media-buttons-control.switch-group-settings-button"
	/>
));

export const ExpressionAutocompleteControl = observer(({title}: SwitchGroupSettingsControlProps) => (
	<SwitchGroupSettingsButton
		mode="expression-autocomplete"
		title={title}
		dataFlx="user.advanced-settings-tab.expression-autocomplete-control.configure-button"
		data-flx="user.advanced-settings-tab.advanced-chat-controls.expression-autocomplete-control.switch-group-settings-button"
	/>
));

export const InputButtonsControl = observer(({title}: SwitchGroupSettingsControlProps) => (
	<SwitchGroupSettingsButton
		mode="input-buttons"
		title={title}
		dataFlx="user.advanced-settings-tab.input-buttons-control.configure-button"
		data-flx="user.advanced-settings-tab.advanced-chat-controls.input-buttons-control.switch-group-settings-button"
	/>
));

export const PreuploadMessageAttachmentsControl = observer(() => {
	const {i18n} = useLingui();
	return (
		<Switch
			ariaLabel={i18n._(UPLOAD_ATTACHMENTS_BEFORE_SENDING_DESCRIPTOR)}
			value={PrivacyPreferences.getPreuploadMessageAttachments()}
			onChange={PrivacyPreferences.setPreuploadMessageAttachments}
			compact
			data-flx="user.advanced-settings-tab.switch.preupload-message-attachments"
		/>
	);
});

export const SequentialFileSendControl = observer(() => {
	const {i18n} = useLingui();
	return (
		<Switch
			ariaLabel={i18n._(SEND_FILES_IN_SEQUENTIAL_ORDER_DESCRIPTOR)}
			value={Accessibility.sequentialFileSend}
			onChange={(value) => AccessibilityCommands.update({sequentialFileSend: value})}
			compact
			data-flx="user.advanced-settings-tab.switch.sequential-file-send"
		/>
	);
});

export const ScrollToBottomOnSendControl = observer(() => {
	const {i18n} = useLingui();
	return (
		<Switch
			ariaLabel={i18n._(SCROLL_TO_BOTTOM_WHEN_SENDING_A_MESSAGE_DESCRIPTOR)}
			value={Accessibility.scrollToBottomOnMessageSend}
			onChange={(value) => AccessibilityCommands.update({scrollToBottomOnMessageSend: value})}
			compact
			data-flx="user.advanced-settings-tab.switch.scroll-to-bottom-on-send"
		/>
	);
});

export const SkipMarkAllAsReadControl = observer(() => {
	const {i18n} = useLingui();
	return (
		<Switch
			ariaLabel={i18n._(SKIP_MARK_ALL_AS_READ_CONFIRMATION_DESCRIPTOR)}
			value={Inbox.skipMarkAllAsReadConfirmation}
			onChange={(value) => Inbox.setSkipMarkAllAsReadConfirmation(value)}
			compact
			data-flx="user.advanced-settings-tab.switch.skip-mark-all-as-read-confirmation"
		/>
	);
});

export const HideMutedChannelsByDefaultControl = observer(() => {
	const {i18n} = useLingui();
	const handleChange = useCallback(
		(value: boolean) => {
			if (value) {
				ModalCommands.push(
					modal(() => (
						<ConfirmModal
							title={i18n._(HIDE_MUTED_CHANNELS_BY_DEFAULT_PROMPT_DESCRIPTOR)}
							description={
								<Trans>
									New communities you join will automatically have muted channels hidden. Would you also like to apply
									this setting to all your existing communities?
								</Trans>
							}
							primaryText={i18n._(APPLY_TO_ALL_COMMUNITIES_DESCRIPTOR)}
							secondaryText={i18n._(NEW_COMMUNITIES_ONLY_DESCRIPTOR)}
							onPrimary={async () => {
								await UserSettingsCommands.update({defaultHideMutedChannels: true});
								const guildIds = Guilds.getGuildIds();
								for (const guildId of guildIds) {
									UserGuildSettingsCommands.updateGuildSettings(
										guildId,
										{hide_muted_channels: true},
										{persistImmediately: true},
									);
								}
							}}
							onSecondary={async () => {
								await UserSettingsCommands.update({defaultHideMutedChannels: true});
							}}
							data-flx="user.advanced-settings-tab.hide-muted-channels.confirm-modal"
						/>
					)),
				);
			} else {
				ModalCommands.push(
					modal(() => (
						<ConfirmModal
							title={i18n._(STOP_HIDING_MUTED_CHANNELS_BY_DEFAULT_DESCRIPTOR)}
							description={
								<Trans>
									New communities you join will no longer have muted channels hidden automatically. Would you also like
									to show muted channels in all your existing communities?
								</Trans>
							}
							primaryText={i18n._(SHOW_IN_ALL_COMMUNITIES_DESCRIPTOR)}
							secondaryText={i18n._(NEW_COMMUNITIES_ONLY_DESCRIPTOR)}
							onPrimary={async () => {
								await UserSettingsCommands.update({defaultHideMutedChannels: false});
								const guildIds = Guilds.getGuildIds();
								for (const guildId of guildIds) {
									UserGuildSettingsCommands.updateGuildSettings(
										guildId,
										{hide_muted_channels: false},
										{persistImmediately: true},
									);
								}
							}}
							onSecondary={async () => {
								await UserSettingsCommands.update({defaultHideMutedChannels: false});
							}}
							data-flx="user.advanced-settings-tab.hide-muted-channels.confirm-modal--2"
						/>
					)),
				);
			}
		},
		[i18n],
	);
	return (
		<Switch
			ariaLabel={i18n._(HIDE_MUTED_CHANNELS_BY_DEFAULT_DESCRIPTOR)}
			value={UserSettings.getDefaultHideMutedChannels()}
			onChange={handleChange}
			compact
			data-flx="user.advanced-settings-tab.switch.hide-muted-channels"
		/>
	);
});
