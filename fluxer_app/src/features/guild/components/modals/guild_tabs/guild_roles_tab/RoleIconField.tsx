// SPDX-License-Identifier: AGPL-3.0-or-later

import {getRoleColor} from '@app/features/app/components/dialogs/shared/PermissionComponents';
import {Message} from '@app/features/channel/components/ChannelMessage';
import {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import {EmojiPickerPopout} from '@app/features/emoji/components/popouts/EmojiPickerPopout';
import type {FlatEmoji, UnicodeEmoji} from '@app/features/emoji/types/EmojiTypes';
import {AssetCropModal, AssetType} from '@app/features/expressions/components/modals/AssetCropModal';
import {inspectImageFile} from '@app/features/expressions/utils/AnimatedImageUtils';
import {buildCustomEmojiURL} from '@app/features/expressions/utils/CustomEmojiImageUrl';
import {containImageInSquare} from '@app/features/expressions/utils/ImageCropUtils';
import UnicodeEmojis from '@app/features/expressions/utils/UnicodeEmojis';
import styles from '@app/features/guild/components/modals/guild_tabs/guild_roles_tab/RoleIconField.module.css';
import type {RoleUpdate} from '@app/features/guild/components/modals/guild_tabs/guild_roles_tab/shared';
import {RoleIcon} from '@app/features/guild/components/RoleIcon';
import type {GuildRole} from '@app/features/guild/models/GuildRole';
import {Message as MessageModel} from '@app/features/messaging/models/MessagingMessage';
import {wrapDesktopLocalResourceURL} from '@app/features/messaging/utils/DesktopResourceUrl';
import {openFilePicker} from '@app/features/messaging/utils/FilePickerUtils';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import {toReactionEmoji} from '@app/features/messaging/utils/ReactionUtils';
import {readBlobAsDataUrl} from '@app/features/platform/utils/BlobDataUrl';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {
	formatAssetUploadExtensions,
	getAcceptString,
	getPolicy,
	isExtensionAllowed,
} from '@fluxer/constants/src/AssetFormatPolicy';
import {MessagePreviewContext, MessageStates, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ImageIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {type DragEvent, useEffect, useId, useMemo, useRef, useState} from 'react';

const ROLE_ICON_DESCRIPTOR = msg({
	message: 'Role icon',
	comment: 'Label of the role icon field in the community role editor.',
});
const ROLE_ICON_HELP_DESCRIPTOR = msg({
	message: 'Upload an image or pick an emoji. Members get the icon of their highest role that has one.',
	comment: 'Helper text under the role icon label in the community role editor.',
});
const CHOOSE_EMOJI_DESCRIPTOR = msg({
	message: 'Choose emoji',
	comment: 'Button in the role icon field that opens the emoji picker. Keep it short.',
});
const ROLE_ICON_UPLOAD_HINT_DESCRIPTOR = msg({
	message: '{formats}. At least {minimumSize}, up to {maxSize}.',
	comment:
		'Helper line under the role icon buttons in the community role editor. formats is a list of image types such as PNG, JPEG, WebP. minimumSize is an image size such as 32×32px. maxSize is a file size such as 256 KB.',
});
const UPLOAD_ROLE_ICON_DESCRIPTOR = msg({
	message: 'Upload role icon',
	comment:
		'Accessible label of the empty role icon tile in the community role editor. Clicking it opens the file picker.',
});
const CURRENT_EMOJI_ROLE_ICON_DESCRIPTOR = msg({
	message: 'Current role icon, {emojiName}',
	comment:
		'Accessible label of the role icon tile in the community role editor when the icon is an emoji, also read out after an emoji is picked. emojiName is the emoji short name such as :shield:. Clicking the tile opens the file picker.',
});
const CURRENT_IMAGE_ROLE_ICON_DESCRIPTOR = msg({
	message: 'Current role icon, uploaded image',
	comment:
		'Accessible label of the role icon tile in the community role editor when the icon is an uploaded image, also read out after an image is added. Clicking the tile opens the file picker.',
});
const USAGE_PREVIEW_MESSAGE_DESCRIPTOR = msg({
	message: 'This is how messages appear',
	comment: 'Sample message text in the role icon preview of the community role editor.',
});
const ANIMATED_ROLE_ICON_DESCRIPTOR = msg({
	message: 'Animated icons are not supported. Use a static image.',
	comment: 'Error under the role icon buttons when the picked image is animated.',
});
const UNSUPPORTED_ROLE_ICON_TYPE_DESCRIPTOR = msg({
	message: 'This file type is not supported. Supported types are {formats}.',
	comment:
		'Error under the role icon buttons when the picked file has an unsupported type. formats is a list such as PNG, JPEG, WebP.',
});
const IMAGE_COULD_NOT_BE_LOADED_DESCRIPTOR = msg({
	message: 'This image could not be loaded. Try another one.',
	comment: 'Error under the role icon buttons when the picked image or emoji could not be used.',
});

const ROLE_ICON_POLICY = getPolicy('role_icon');
const ROLE_ICON_MAX_BYTES = ROLE_ICON_POLICY.maxBytes;
const ROLE_ICON_MIN_DIMENSION = ROLE_ICON_POLICY.dims?.min ?? 0;
const CUSTOM_EMOJI_ICON_SIZE = 128;
const USAGE_PREVIEW_CHANNEL_ID = '1000000000000000060';
const USAGE_PREVIEW_MESSAGE_ID = '1000000000000000061';

interface RoleIconFieldProps {
	role: GuildRole;
	disabled: boolean;
	onChange: (updates: Pick<RoleUpdate, 'icon' | 'unicodeEmoji'>) => void;
}

async function fetchCustomEmojiDataUrl(id: string): Promise<string> {
	const response = await fetch(
		wrapDesktopLocalResourceURL(buildCustomEmojiURL({id, animated: false, size: CUSTOM_EMOJI_ICON_SIZE})),
	);
	if (!response.ok) {
		throw new Error(`Emoji image request failed with status ${response.status}`);
	}
	const square = await containImageInSquare(await response.blob(), CUSTOM_EMOJI_ICON_SIZE, ROLE_ICON_MAX_BYTES);
	return readBlobAsDataUrl(square);
}

async function raiseToMinimumSize(image: Blob): Promise<Blob> {
	const bitmap = await createImageBitmap(image);
	const {width, height} = bitmap;
	bitmap.close();
	if (width >= ROLE_ICON_MIN_DIMENSION && height >= ROLE_ICON_MIN_DIMENSION) {
		return image;
	}
	return containImageInSquare(image, ROLE_ICON_MIN_DIMENSION * 2, ROLE_ICON_MAX_BYTES);
}

function getTileLabel(i18n: I18n, role: GuildRole): string {
	if (role.icon) {
		return i18n._(CURRENT_IMAGE_ROLE_ICON_DESCRIPTOR);
	}
	if (role.unicodeEmoji) {
		const surrogate = UnicodeEmojis.normalizeEmojiNameToSurrogate(role.unicodeEmoji);
		return i18n._(CURRENT_EMOJI_ROLE_ICON_DESCRIPTOR, {
			emojiName: UnicodeEmojis.nameForSurrogate(surrogate, true, role.unicodeEmoji),
		});
	}
	return i18n._(UPLOAD_ROLE_ICON_DESCRIPTOR);
}

const RoleIconUsagePreview = observer(function RoleIconUsagePreview({role, user}: {role: GuildRole; user: User}) {
	const {i18n} = useLingui();
	const channel = useMemo(
		() =>
			new Channel({
				id: USAGE_PREVIEW_CHANNEL_ID,
				type: 0,
				name: 'role-icon-preview',
				position: 0,
				parent_id: null,
				topic: null,
				url: null,
				nsfw: false,
				last_message_id: null,
				last_pin_timestamp: null,
				bitrate: null,
				user_limit: null,
				permission_overwrites: [],
			}),
		[],
	);
	useEffect(() => {
		Channels.handleChannelCreate({channel: channel.toJSON()});
		return () => {
			Channels.handleChannelDelete({channel: channel.toJSON()});
		};
	}, [channel]);
	const message = useMemo(
		() =>
			new MessageModel(
				{
					id: USAGE_PREVIEW_MESSAGE_ID,
					channel_id: USAGE_PREVIEW_CHANNEL_ID,
					author: user.toJSON(),
					type: MessageTypes.DEFAULT,
					flags: 0,
					pinned: false,
					mention_everyone: false,
					content: i18n._(USAGE_PREVIEW_MESSAGE_DESCRIPTOR),
					timestamp: new Date().toISOString(),
					state: MessageStates.SENT,
				},
				{skipUserCache: true},
			),
		[user, i18n.locale],
	);
	const previewOverrides = useMemo(
		() => ({
			usernameColor: role.color === 0 ? undefined : getRoleColor(role.color),
			displayName: NicknameUtils.getNickname(user, role.guildId),
			iconRole: role,
		}),
		[role, user],
	);
	return (
		<div
			className={styles.usagePreview}
			aria-hidden="true"
			inert={true}
			data-flx="guild.guild-tabs.guild-roles-tab.role-icon-field.usage-preview"
		>
			{(['dark', 'light'] as const).map((theme) => (
				<div
					key={theme}
					className={clsx(`theme-${theme}`, styles.usagePreviewSurface)}
					data-flx={`guild.guild-tabs.guild-roles-tab.role-icon-field.usage-preview.${theme}`}
				>
					<Message
						channel={channel}
						message={message}
						compact={false}
						removeTopSpacing={true}
						previewContext={MessagePreviewContext.SETTINGS}
						previewOverrides={previewOverrides}
						data-flx="guild.guild-tabs.guild-roles-tab.role-icon-field.usage-preview.message"
					/>
				</div>
			))}
		</div>
	);
});

export const RoleIconField = observer(function RoleIconField({role, disabled, onChange}: RoleIconFieldProps) {
	const {i18n} = useLingui();
	const [error, setError] = useState<MessageDescriptor | null>(null);
	const [isDraggingFile, setIsDraggingFile] = useState(false);
	const [announcement, setAnnouncement] = useState('');
	const tileRef = useRef<HTMLButtonElement>(null);
	const focusTileRef = useRef(false);
	const announceRef = useRef(false);
	const stageTokenRef = useRef(0);
	const labelId = useId();
	const descriptionId = useId();
	const hintId = useId();
	const hasIcon = role.icon !== null || role.unicodeEmoji !== null;
	const currentIconLabel = getTileLabel(i18n, role);
	const currentUser = Users.getCurrentUser();
	const formats = formatAssetUploadExtensions('role_icon');
	useEffect(() => {
		setError(null);
		if (announceRef.current && hasIcon) {
			setAnnouncement(currentIconLabel);
		}
		announceRef.current = false;
	}, [role.icon, role.unicodeEmoji]);
	useEffect(() => {
		if (!hasIcon && focusTileRef.current) {
			focusTileRef.current = false;
			tileRef.current?.focus();
		}
	}, [hasIcon]);
	const uploadHint = i18n._(ROLE_ICON_UPLOAD_HINT_DESCRIPTOR, {
		formats,
		minimumSize: `${ROLE_ICON_MIN_DIMENSION}×${ROLE_ICON_MIN_DIMENSION}px`,
		maxSize: formatFileSize(i18n.locale, ROLE_ICON_MAX_BYTES),
	});
	const stageImage = (dataUrl: string) => {
		stageTokenRef.current += 1;
		setError(null);
		announceRef.current = true;
		onChange({icon: dataUrl, unicodeEmoji: null});
	};
	const handleFile = async (file: File) => {
		stageTokenRef.current += 1;
		setError(null);
		const {format, animated} = await inspectImageFile(file);
		if (!isExtensionAllowed('role_icon', format)) {
			setError(UNSUPPORTED_ROLE_ICON_TYPE_DESCRIPTOR);
			return;
		}
		if (animated) {
			setError(ANIMATED_ROLE_ICON_DESCRIPTOR);
			return;
		}
		let source: string;
		try {
			source = await readBlobAsDataUrl(file);
		} catch {
			setError(IMAGE_COULD_NOT_BE_LOADED_DESCRIPTOR);
			return;
		}
		ModalCommands.push(
			modal(() => (
				<AssetCropModal
					assetType={AssetType.ROLE_ICON}
					imageUrl={source}
					onCropComplete={async (croppedBlob) => {
						const token = ++stageTokenRef.current;
						try {
							const dataUrl = await readBlobAsDataUrl(await raiseToMinimumSize(croppedBlob));
							if (token === stageTokenRef.current) {
								stageImage(dataUrl);
							}
						} catch {
							if (token === stageTokenRef.current) {
								setError(IMAGE_COULD_NOT_BE_LOADED_DESCRIPTOR);
							}
						}
					}}
					onSkip={file.size <= ROLE_ICON_MAX_BYTES ? () => stageImage(source) : undefined}
					data-flx="guild.guild-tabs.guild-roles-tab.role-icon-field.handle-file.asset-crop-modal"
				/>
			)),
		);
	};
	const handleUpload = async () => {
		const [file] = await openFilePicker({accept: getAcceptString('role_icon')});
		if (file) {
			await handleFile(file);
		}
	};
	const handleDragOver = (event: DragEvent<HTMLButtonElement>) => {
		if (disabled || !event.dataTransfer.types.includes('Files')) {
			return;
		}
		event.preventDefault();
		event.stopPropagation();
		event.dataTransfer.dropEffect = 'copy';
		setIsDraggingFile(true);
	};
	const handleDrop = async (event: DragEvent<HTMLButtonElement>) => {
		setIsDraggingFile(false);
		if (disabled) {
			return;
		}
		event.preventDefault();
		event.stopPropagation();
		const [file] = event.dataTransfer.files;
		if (file) {
			await handleFile(file);
		}
	};
	const handleEmojiSelect = async (emoji: FlatEmoji) => {
		setError(null);
		const token = ++stageTokenRef.current;
		if (!emoji.id) {
			announceRef.current = true;
			onChange({icon: null, unicodeEmoji: toReactionEmoji(emoji as UnicodeEmoji).name});
			return;
		}
		try {
			const dataUrl = await fetchCustomEmojiDataUrl(emoji.id);
			if (token === stageTokenRef.current) {
				stageImage(dataUrl);
			}
		} catch {
			if (token === stageTokenRef.current) {
				setError(IMAGE_COULD_NOT_BE_LOADED_DESCRIPTOR);
			}
		}
	};
	return (
		<div
			className={styles.field}
			role="group"
			aria-labelledby={labelId}
			aria-describedby={`${descriptionId} ${hintId}`}
			data-flx="guild.guild-tabs.guild-roles-tab.role-icon-field.field"
		>
			<div data-flx="guild.guild-tabs.guild-roles-tab.role-icon-field.div">
				<p id={labelId} className={styles.label} data-flx="guild.guild-tabs.guild-roles-tab.role-icon-field.label">
					{i18n._(ROLE_ICON_DESCRIPTOR)}
				</p>
				<p
					id={descriptionId}
					className={styles.description}
					data-flx="guild.guild-tabs.guild-roles-tab.role-icon-field.description"
				>
					{i18n._(ROLE_ICON_HELP_DESCRIPTOR)}
				</p>
			</div>
			<div className={styles.body} data-flx="guild.guild-tabs.guild-roles-tab.role-icon-field.body">
				<FocusRing offset={-2} data-flx="guild.guild-tabs.guild-roles-tab.role-icon-field.focus-ring">
					<button
						ref={tileRef}
						type="button"
						className={clsx(hasIcon ? styles.preview : styles.emptyPreview, isDraggingFile && styles.previewDragging)}
						onClick={handleUpload}
						onDragOver={handleDragOver}
						onDragLeave={() => setIsDraggingFile(false)}
						onDrop={handleDrop}
						disabled={disabled}
						aria-label={currentIconLabel}
						data-flx="guild.guild-tabs.guild-roles-tab.role-icon-field.tile.upload"
					>
						{hasIcon ? (
							<RoleIcon
								role={role}
								size={40}
								decorative={true}
								data-flx="guild.guild-tabs.guild-roles-tab.role-icon-field.role-icon"
							/>
						) : (
							<ImageIcon
								size={24}
								aria-hidden="true"
								data-flx="guild.guild-tabs.guild-roles-tab.role-icon-field.image-icon"
							/>
						)}
					</button>
				</FocusRing>
				<div className={styles.controls} data-flx="guild.guild-tabs.guild-roles-tab.role-icon-field.controls">
					<div className={styles.buttons} data-flx="guild.guild-tabs.guild-roles-tab.role-icon-field.buttons">
						<Button
							variant="secondary"
							onClick={handleUpload}
							disabled={disabled}
							data-flx="guild.guild-tabs.guild-roles-tab.role-icon-field.button.upload"
						>
							<Trans>Upload image</Trans>
						</Button>
						<Popout
							position="bottom-start"
							offsetMainAxis={8}
							render={({onClose}) => (
								<EmojiPickerPopout
									channelId={null}
									roleIconGuildId={role.guildId}
									handleSelect={handleEmojiSelect}
									onClose={onClose}
									data-flx="guild.guild-tabs.guild-roles-tab.role-icon-field.emoji-picker-popout"
								/>
							)}
							data-flx="guild.guild-tabs.guild-roles-tab.role-icon-field.popout"
						>
							<Button
								variant="secondary"
								disabled={disabled}
								data-flx="guild.guild-tabs.guild-roles-tab.role-icon-field.button.choose-emoji"
							>
								{i18n._(CHOOSE_EMOJI_DESCRIPTOR)}
							</Button>
						</Popout>
						{hasIcon && (
							<Button
								variant="secondary"
								onClick={() => {
									stageTokenRef.current += 1;
									setError(null);
									focusTileRef.current = true;
									onChange({icon: null, unicodeEmoji: null});
								}}
								disabled={disabled}
								data-flx="guild.guild-tabs.guild-roles-tab.role-icon-field.button.remove-icon"
							>
								<Trans>Remove icon</Trans>
							</Button>
						)}
					</div>
					{error && (
						<p role="alert" className={styles.error} data-flx="guild.guild-tabs.guild-roles-tab.role-icon-field.error">
							{i18n._(error, {formats})}
						</p>
					)}
					<p id={hintId} className={styles.hint} data-flx="guild.guild-tabs.guild-roles-tab.role-icon-field.hint">
						{uploadHint}
					</p>
					<span
						className={styles.liveRegion}
						aria-live="polite"
						data-flx="guild.guild-tabs.guild-roles-tab.role-icon-field.live-region"
					>
						{announcement}
					</span>
				</div>
			</div>
			{currentUser && (
				<RoleIconUsagePreview
					role={role}
					user={currentUser}
					data-flx="guild.guild-tabs.guild-roles-tab.role-icon-field.role-icon-usage-preview"
				/>
			)}
		</div>
	);
});
