// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AccessibilityCommands from '@app/features/accessibility/commands/AccessibilityCommands';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {CANNOT_SEND_MESSAGES_IN_CHANNEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Keybind from '@app/features/input/state/InputKeybind';
import * as PremiumModalCommands from '@app/features/premium/commands/PremiumModalCommands';
import {CheckboxItem} from '@app/features/ui/action_menu/ContextMenu';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import {KeybindHint} from '@app/features/ui/keybind_hint/KeybindHint';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {GiftIcon, MicrophoneIcon, PaperclipIcon, TableIcon, UploadSimpleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

const YOU_DO_NOT_HAVE_PERMISSION_TO_UPLOAD_FILES_DESCRIPTOR = msg({
	message: "You can't upload files in this channel.",
	comment: 'Tooltip on the disabled upload file item when the user lacks Attach Files permission. Calm, factual tone.',
});
export const UPLOAD_FILE_DESCRIPTOR = msg({
	message: 'Upload file',
	comment: 'Plus menu item that opens the system file picker to attach a file.',
});
const CANNOT_CREATE_POLL_DESCRIPTOR = msg({
	message: "You can't create polls in this channel.",
	comment: 'Tooltip on the disabled create poll item when the user lacks Create Poll permission. Calm, factual tone.',
});
export const CREATE_POLL_DESCRIPTOR = msg({
	message: 'Create poll',
	comment: 'Plus menu item that opens a form to create a poll.',
});
export const UPLOAD_YOUR_MESSAGE_AS_A_FILE_DESCRIPTOR = msg({
	message: 'Upload your message as a file',
	comment: 'Plus menu item that uploads the current textarea content as a .txt file when it exceeds the size limit.',
});
export const SEND_GIFT_DESCRIPTOR = msg({
	message: 'Send gift',
	comment: 'Plus menu item that opens the gift purchase flow.',
});
const SEND_VOICE_MESSAGE_DESCRIPTOR = msg({
	message: 'Send voice message',
	comment: 'Plus menu item that opens the desktop voice message composer to record and send a voice clip.',
});
const CUSTOMIZE_DESCRIPTOR = msg({
	message: 'Customize',
	comment: 'Plus menu submenu label for visibility toggles on textarea adornment buttons.',
});
const SHOW_GIFS_BUTTON_DESCRIPTOR = msg({
	message: 'Show GIFs button',
	comment: 'Plus menu submenu toggle that shows the GIFs button next to the textarea.',
});
const SHOW_MEDIA_BUTTON_DESCRIPTOR = msg({
	message: 'Show media button',
	comment: 'Plus menu submenu toggle that shows the saved media (memes) button next to the textarea.',
});
const SHOW_STICKERS_BUTTON_DESCRIPTOR = msg({
	message: 'Show stickers button',
	comment: 'Short label in the channel and chat textarea plus menu. Keep it concise.',
});
const SHOW_EMOJI_BUTTON_DESCRIPTOR = msg({
	message: 'Show emoji button',
	comment: 'Short label in the channel and chat textarea plus menu. Keep it concise.',
});
const SHOW_SEND_BUTTON_DESCRIPTOR = msg({
	message: 'Show send button',
	comment: 'Short label in the channel and chat textarea plus menu. Keep it concise.',
});

interface TextareaPlusMenuProps {
	onUploadFile: () => void;
	onSendPoll: () => void;
	onSchedule?: () => void;
	canSendPolls?: boolean;
	canSchedule?: boolean;
	canAttachFiles: boolean;
	canSendMessages: boolean;
	textareaValue?: string;
	onUploadAsFile?: () => void;
	onSendVoiceMessage?: () => void;
}

export const TextareaPlusMenu = observer(
	({
		onUploadFile,
		onSendPoll,
		canSendPolls,
		canAttachFiles,
		canSendMessages,
		textareaValue,
		onUploadAsFile,
		onSendVoiceMessage,
	}: TextareaPlusMenuProps) => {
		const {i18n} = useLingui();
		const showGifButton = Accessibility.showGifButton;
		const showMemesButton = Accessibility.showMemesButton;
		const showStickersButton = Accessibility.showStickersButton;
		const showEmojiButton = Accessibility.showEmojiButton;
		const showMessageSendButton = Accessibility.showMessageSendButton;
		const isSelfHosted = RuntimeConfig.isSelfHosted();
		const hasTextContent = textareaValue && textareaValue.trim().length > 0;
		const cannotSendMessagesHint = i18n._(CANNOT_SEND_MESSAGES_IN_CHANNEL_DESCRIPTOR);
		const cannotUploadFilesHint = i18n._(YOU_DO_NOT_HAVE_PERMISSION_TO_UPLOAD_FILES_DESCRIPTOR);
		const cannotCreatePollHint = i18n._(CANNOT_CREATE_POLL_DESCRIPTOR);
		let uploadActionHint: string | undefined;
		if (!canSendMessages) {
			uploadActionHint = cannotSendMessagesHint;
		} else if (!canAttachFiles) {
			uploadActionHint = cannotUploadFilesHint;
		}
		const createPollHint = !canSendMessages ? cannotSendMessagesHint : !canSendPolls ? cannotCreatePollHint : undefined;
		const sendGiftHint = !canSendMessages ? cannotSendMessagesHint : undefined;
		const sendVoiceMessageDisabledHint = !canSendMessages
			? cannotSendMessagesHint
			: !canAttachFiles
				? cannotUploadFilesHint
				: undefined;
		const sendVoiceMessageKeybind = Keybind.getByAction('chat_send_voice_message').combo;
		const sendVoiceMessageKeybindHint =
			sendVoiceMessageKeybind.key || sendVoiceMessageKeybind.code ? (
				<KeybindHint
					combo={sendVoiceMessageKeybind}
					data-flx="channel.textarea.textarea-plus-menu.send-voice-message-keybind-hint"
				/>
			) : undefined;
		return (
			<MenuGroup data-flx="channel.textarea.textarea-plus-menu.menu-group">
				<MenuItem
					icon={<PaperclipIcon weight="bold" data-flx="channel.textarea.textarea-plus-menu.paperclip-icon" />}
					onClick={onUploadFile}
					disabled={uploadActionHint != null}
					hint={uploadActionHint}
					data-flx="channel.textarea.textarea-plus-menu.menu-item.upload-file"
				>
					{i18n._(UPLOAD_FILE_DESCRIPTOR)}
				</MenuItem>
				{hasTextContent && onUploadAsFile && (
					<MenuItem
						icon={<UploadSimpleIcon data-flx="channel.textarea.textarea-plus-menu.upload-simple-icon" />}
						onClick={onUploadAsFile}
						disabled={uploadActionHint != null}
						hint={uploadActionHint}
						data-flx="channel.textarea.textarea-plus-menu.menu-item.upload-as-file"
					>
						{i18n._(UPLOAD_YOUR_MESSAGE_AS_A_FILE_DESCRIPTOR)}
					</MenuItem>
				)}
				<MenuItem
					icon={<TableIcon weight="bold" data-flx="channel.textarea.textarea-plus-menu.table-icon" />}
					onClick={onSendPoll}
					disabled={createPollHint != null}
					hint={createPollHint}
					data-flx="channel.textarea.textarea-plus-menu.menu-item.create-poll"
				>
					{i18n._(CREATE_POLL_DESCRIPTOR)}
				</MenuItem>
				{!isSelfHosted && (
					<MenuItem
						icon={<GiftIcon data-flx="channel.textarea.textarea-plus-menu.gift-icon" />}
						onClick={() => PremiumModalCommands.open(true)}
						disabled={sendGiftHint != null}
						hint={sendGiftHint}
						data-flx="channel.textarea.textarea-plus-menu.menu-item.open"
					>
						{i18n._(SEND_GIFT_DESCRIPTOR)}
					</MenuItem>
				)}
				{onSendVoiceMessage && (
					<MenuItem
						icon={<MicrophoneIcon weight="bold" data-flx="channel.textarea.textarea-plus-menu.microphone-icon" />}
						onClick={onSendVoiceMessage}
						disabled={!canSendMessages || !canAttachFiles}
						hint={sendVoiceMessageDisabledHint ?? sendVoiceMessageKeybindHint}
						data-flx="channel.textarea.textarea-plus-menu.menu-item.send-voice-message"
					>
						{i18n._(SEND_VOICE_MESSAGE_DESCRIPTOR)}
					</MenuItem>
				)}
				<MenuItemSubmenu
					label={i18n._(CUSTOMIZE_DESCRIPTOR)}
					render={() => (
						<>
							<MenuGroup data-flx="channel.textarea.textarea-plus-menu.menu-group--2">
								<CheckboxItem
									checked={showGifButton}
									onCheckedChange={(checked) => AccessibilityCommands.update({showGifButton: checked})}
									closeOnChange={false}
									data-flx="channel.textarea.textarea-plus-menu.checkbox-item"
								>
									{i18n._(SHOW_GIFS_BUTTON_DESCRIPTOR)}
								</CheckboxItem>
								<CheckboxItem
									checked={showMemesButton}
									onCheckedChange={(checked) => AccessibilityCommands.update({showMemesButton: checked})}
									closeOnChange={false}
									data-flx="channel.textarea.textarea-plus-menu.checkbox-item--2"
								>
									{i18n._(SHOW_MEDIA_BUTTON_DESCRIPTOR)}
								</CheckboxItem>
								<CheckboxItem
									checked={showStickersButton}
									onCheckedChange={(checked) => AccessibilityCommands.update({showStickersButton: checked})}
									closeOnChange={false}
									data-flx="channel.textarea.textarea-plus-menu.checkbox-item--3"
								>
									{i18n._(SHOW_STICKERS_BUTTON_DESCRIPTOR)}
								</CheckboxItem>
								<CheckboxItem
									checked={showEmojiButton}
									onCheckedChange={(checked) => AccessibilityCommands.update({showEmojiButton: checked})}
									closeOnChange={false}
									data-flx="channel.textarea.textarea-plus-menu.checkbox-item--4"
								>
									{i18n._(SHOW_EMOJI_BUTTON_DESCRIPTOR)}
								</CheckboxItem>
								<CheckboxItem
									checked={showMessageSendButton}
									onCheckedChange={(checked) => AccessibilityCommands.update({showMessageSendButton: checked})}
									closeOnChange={false}
									data-flx="channel.textarea.textarea-plus-menu.checkbox-item--5"
								>
									{i18n._(SHOW_SEND_BUTTON_DESCRIPTOR)}
								</CheckboxItem>
							</MenuGroup>
						</>
					)}
					data-flx="channel.textarea.textarea-plus-menu.menu-item-submenu"
				/>
			</MenuGroup>
		);
	},
);
