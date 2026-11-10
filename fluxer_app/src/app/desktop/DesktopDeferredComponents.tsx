// SPDX-License-Identifier: AGPL-3.0-or-later

import {createBootDeferredCommand} from '@app/app/desktop/BootDeferredCommand';
import {createBootDeferredComponent} from '@app/app/desktop/BootDeferredComponent';
import type {ComponentProps, ComponentType} from 'react';

type DeferredUI = typeof import('./DesktopDeferredUI');
type ComponentName = {
	[Name in keyof DeferredUI]: DeferredUI[Name] extends ComponentType<infer _Props> ? Name : never;
}[keyof DeferredUI];
type CommandName = Exclude<keyof DeferredUI, ComponentName>;

function deferredComponent<Name extends ComponentName>(name: Name, load: () => Promise<DeferredUI[Name]>) {
	type Props = ComponentProps<DeferredUI[Name]> & object;
	return createBootDeferredComponent<Props>(name, async () => (await load()) as ComponentType<Props>);
}

function deferredCommand<Name extends CommandName>(load: () => Promise<DeferredUI[Name]>) {
	type Args = Parameters<DeferredUI[Name]>;
	return createBootDeferredCommand<Args>(async () => (await load()) as (...args: Args) => void);
}

export const PremiumCheckoutReturnWatcher = deferredComponent(
	'PremiumCheckoutReturnWatcher',
	async () =>
		(await import('../../features/premium/components/PremiumCheckoutReturnWatcher')).PremiumCheckoutReturnWatcher,
);
export const EmojiPicker = deferredComponent(
	'EmojiPicker',
	async () => (await import('../../features/channel/components/EmojiPicker')).EmojiPicker,
);
export const StickersPicker = deferredComponent(
	'StickersPicker',
	async () => (await import('../../features/channel/components/StickersPicker')).StickersPicker,
);
export const GifPicker = deferredComponent(
	'GifPicker',
	async () => (await import('../../features/channel/components/pickers/gif/GifPicker')).GifPicker,
);
export const MemesPicker = deferredComponent(
	'MemesPicker',
	async () => (await import('../../features/channel/components/pickers/memes/MemesPicker')).MemesPicker,
);
export const ExpressionPickerSheet = deferredComponent(
	'ExpressionPickerSheet',
	async () =>
		(await import('../../features/expressions/components/modals/ExpressionPickerSheet')).ExpressionPickerSheet,
);
export const UserContextMenu = deferredComponent(
	'UserContextMenu',
	async () => (await import('../../features/ui/action_menu/UserContextMenu')).UserContextMenu,
);
export const GuildMemberContextMenu = deferredComponent(
	'GuildMemberContextMenu',
	async () => (await import('../../features/ui/action_menu/GuildMemberContextMenu')).GuildMemberContextMenu,
);
export const MessageContextMenu = deferredComponent(
	'MessageContextMenu',
	async () => (await import('../../features/ui/action_menu/MessageContextMenu')).MessageContextMenu,
);
export const GuildContextMenu = deferredComponent(
	'GuildContextMenu',
	async () => (await import('../../features/ui/action_menu/GuildContextMenu')).GuildContextMenu,
);
export const ChannelContextMenu = deferredComponent(
	'ChannelContextMenu',
	async () => (await import('../../features/ui/action_menu/ChannelContextMenu')).ChannelContextMenu,
);
export const ConfirmModal = deferredComponent(
	'ConfirmModal',
	async () => (await import('../../features/app/components/dialogs/ConfirmModal')).ConfirmModal,
);
export const OAuthAuthorizeModal = deferredComponent(
	'OAuthAuthorizeModal',
	async () => (await import('../../features/auth/components/modals/OAuthAuthorizeModal')).OAuthAuthorizeModal,
);
export const UserProfileModal = deferredComponent(
	'UserProfileModal',
	async () => (await import('../../features/user/components/modals/UserProfileModal')).UserProfileModal,
);
export const AddGuildModal = deferredComponent(
	'AddGuildModal',
	async () => (await import('../../features/guild/components/modals/AddGuildModal')).AddGuildModal,
);
export const ForwardModal = deferredComponent(
	'ForwardModal',
	async () => (await import('../../features/messaging/components/modals/ForwardModal')).ForwardModal,
);
export const ImageCropModal = deferredComponent(
	'ImageCropModal',
	async () => (await import('../../features/messaging/components/modals/ImageCropModal')).ImageCropModal,
);
export const GuildNotificationSettingsModal = deferredComponent(
	'GuildNotificationSettingsModal',
	async () =>
		(await import('../../features/guild/components/modals/GuildNotificationSettingsModal'))
			.GuildNotificationSettingsModal,
);
export const InviteModal = deferredComponent(
	'InviteModal',
	async () => (await import('../../features/invite/components/modals/InviteModal')).InviteModal,
);
export const ChannelCreateModal = deferredComponent(
	'ChannelCreateModal',
	async () => (await import('../../features/channel/components/modals/ChannelCreateModal')).ChannelCreateModal,
);
export const ChannelFollowModal = deferredComponent(
	'ChannelFollowModal',
	async () => (await import('../../features/channel/components/modals/ChannelFollowModal')).ChannelFollowModal,
);
export const EditGroupModal = deferredComponent(
	'EditGroupModal',
	async () => (await import('../../features/channel/components/modals/EditGroupModal')).EditGroupModal,
);
export const DMContextMenu = deferredComponent(
	'DMContextMenu',
	async () => (await import('../../features/ui/action_menu/DMContextMenu')).DMContextMenu,
);
export const GroupDMContextMenu = deferredComponent(
	'GroupDMContextMenu',
	async () => (await import('../../features/ui/action_menu/GroupDMContextMenu')).GroupDMContextMenu,
);
export const GroupDMMemberContextMenu = deferredComponent(
	'GroupDMMemberContextMenu',
	async () => (await import('../../features/ui/action_menu/GroupDMContextMenu')).GroupDMMemberContextMenu,
);
export const VoiceParticipantContextMenu = deferredComponent(
	'VoiceParticipantContextMenu',
	async () => (await import('../../features/ui/action_menu/VoiceParticipantContextMenu')).VoiceParticipantContextMenu,
);
export const MediaContextMenu = deferredComponent(
	'MediaContextMenu',
	async () => (await import('../../features/ui/action_menu/MediaContextMenu')).MediaContextMenu,
);
export const FavoritesChannelContextMenu = deferredComponent(
	'FavoritesChannelContextMenu',
	async () => (await import('../../features/ui/action_menu/FavoritesChannelContextMenu')).FavoritesChannelContextMenu,
);
export const FluxerButtonContextMenu = deferredComponent(
	'FluxerButtonContextMenu',
	async () => (await import('../../features/ui/action_menu/FluxerButtonContextMenu')).FluxerButtonContextMenu,
);
export const SettingsContextMenu = deferredComponent(
	'SettingsContextMenu',
	async () => (await import('../../features/ui/action_menu/SettingsContextMenu')).SettingsContextMenu,
);
export const MessageCrosspostConfirmModal = deferredComponent(
	'MessageCrosspostConfirmModal',
	async () =>
		(await import('../../features/messaging/components/modals/MessageCrosspostConfirmModal'))
			.MessageCrosspostConfirmModal,
);
export const GiftPlutoniumModal = deferredComponent(
	'GiftPlutoniumModal',
	async () => (await import('../../features/premium/components/modals/GiftPlutoniumModal')).GiftPlutoniumModal,
);
export const VoiceParticipantBottomSheet = deferredComponent(
	'VoiceParticipantBottomSheet',
	async () =>
		(await import('../../features/voice/components/bottomsheets/VoiceParticipantBottomSheet'))
			.VoiceParticipantBottomSheet,
);
export const GuildHeaderBottomSheet = deferredComponent(
	'GuildHeaderBottomSheet',
	async () =>
		(await import('../../features/guild/components/bottomsheets/GuildHeaderBottomSheet')).GuildHeaderBottomSheet,
);
export const GuildFolderContextMenu = deferredComponent(
	'GuildFolderContextMenu',
	async () => (await import('../../features/ui/action_menu/GuildFolderContextMenu')).GuildFolderContextMenu,
);
export const TextareaContextMenu = deferredComponent(
	'TextareaContextMenu',
	async () => (await import('../../features/channel/components/textarea/TextareaContextMenu')).TextareaContextMenu,
);
export const ChannelDetailsBottomSheet = deferredComponent(
	'ChannelDetailsBottomSheet',
	async () =>
		(await import('../../features/channel/components/bottomsheets/ChannelDetailsBottomSheet'))
			.ChannelDetailsBottomSheet,
);
export const ChannelSearchBottomSheet = deferredComponent(
	'ChannelSearchBottomSheet',
	async () =>
		(await import('../../features/channel/components/bottomsheets/ChannelSearchBottomSheet')).ChannelSearchBottomSheet,
);
export const AddFriendsToGroupModal = deferredComponent(
	'AddFriendsToGroupModal',
	async () =>
		(await import('../../features/relationship/components/modals/AddFriendsToGroupModal')).AddFriendsToGroupModal,
);
export const CustomStatusModal = deferredComponent(
	'CustomStatusModal',
	async () => (await import('../../features/user/components/modals/CustomStatusModal')).CustomStatusModal,
);
export const CreateDMModal = deferredComponent(
	'CreateDMModal',
	async () => (await import('../../features/channel/components/modals/CreateDMModal')).CreateDMModal,
);
export const GroupInvitesModal = deferredComponent(
	'GroupInvitesModal',
	async () => (await import('../../features/channel/components/modals/GroupInvitesModal')).GroupInvitesModal,
);

export const openClaimAccountModal = deferredCommand<'openClaimAccountModal'>(
	async () => (await import('../../features/auth/components/modals/ClaimAccountModal')).openClaimAccountModal,
);
export const openRecoveryKitModal = deferredCommand<'openRecoveryKitModal'>(
	async () => (await import('../../features/auth/components/modals/RecoveryKitModal')).openRecoveryKitModal,
);
export const openChannelFollowModal = deferredCommand<'openChannelFollowModal'>(
	async () => (await import('../../features/channel/components/modals/ChannelFollowModal')).openChannelFollowModal,
);
export const openGuildFolderSettingsModal = deferredCommand<'openGuildFolderSettingsModal'>(
	async () =>
		(await import('../../features/guild/components/modals/GuildFolderSettingsModal')).openGuildFolderSettingsModal,
);

export type {RecoveryKitModalProps} from '../../features/auth/components/modals/RecoveryKitModal';
export type {GifPickerProps} from '../../features/channel/components/pickers/gif/GifPicker';
export type {MemesPickerProps} from '../../features/channel/components/pickers/memes/MemesPicker';
export type {TextareaContextMenuEditFlags} from '../../features/channel/components/textarea/TextareaContextMenu';
export type {AddGuildModalView} from '../../features/guild/components/modals/AddGuildModal';
export type {ForwardModalSuccess} from '../../features/messaging/components/modals/ForwardModal';
