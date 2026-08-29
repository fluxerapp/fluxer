// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getPermissionState,
	type PermissionState,
} from '@app/features/app/components/dialogs/shared/PermissionComponents';
import * as ChannelCommands from '@app/features/channel/commands/ChannelCommands';
import {ChannelPermissionsUpdateFailedModal} from '@app/features/channel/components/alerts/ChannelPermissionsUpdateFailedModal';
import styles from '@app/features/channel/components/modals/channel_tabs/ChannelPermissionsTab.module.css';
import {ChannelPermissionsSidebar} from '@app/features/channel/components/modals/channel_tabs/channel_permissions_tab/ChannelPermissionsSidebar';
import {MobileOverrideListView} from '@app/features/channel/components/modals/channel_tabs/channel_permissions_tab/MobileOverrideListView';
import {PermissionEditorPanel} from '@app/features/channel/components/modals/channel_tabs/channel_permissions_tab/PermissionEditorPanel';
import {SyncWithParentBanner} from '@app/features/channel/components/modals/channel_tabs/channel_permissions_tab/SyncWithParentBanner';
import {
	CHANNEL_PERMISSIONS_TAB_ID,
	channelPermissionsTabLogger,
	type PermissionOverwrite,
} from '@app/features/channel/components/modals/channel_tabs/channel_permissions_tab/shared';
import Channels from '@app/features/channel/state/Channels';
import Guilds from '@app/features/guild/state/Guilds';
import GuildMembers from '@app/features/member/state/GuildMembers';
import Permission from '@app/features/permissions/state/Permission';
import PermissionLayout from '@app/features/permissions/state/PermissionLayout';
import * as PermissionUtils from '@app/features/permissions/utils/PermissionUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import * as UnsavedChangesCommands from '@app/features/ui/commands/UnsavedChangesCommands';
import LayerManager from '@app/features/ui/state/LayerManager';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import SettingsSidebar from '@app/features/ui/state/SettingsSidebar';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {flip, offset, shift, useClick, useDismiss, useFloating, useInteractions, useRole} from '@floating-ui/react';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {matchSorter} from 'match-sorter';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useLayoutEffect, useMemo, useState} from 'react';

const YOU_NEED_THE_PERMISSION_TO_EDIT_THESE_PERMISSIONS_DESCRIPTOR = msg({
	message: 'You need the "{manageChannelsPermissionLabel}" permission to edit these permissions.',
	comment:
		'Inline tooltip on disabled permission checkboxes in the channel permissions tab when the user lacks Manage Channel. manageChannelsPermissionLabel is the localized permission name.',
});
const YOU_NEED_THE_PERMISSION_TO_EDIT_THESE_PERMISSIONS_2_DESCRIPTOR = msg({
	message: 'You need the "{manageRolesPermissionLabel}" permission to edit these permissions.',
	comment:
		'Inline tooltip on disabled permission checkboxes in the channel permissions tab when the user lacks Manage Roles. manageRolesPermissionLabel is the localized permission name.',
});
const YOU_CAN_T_GRANT_A_PERMISSION_YOU_DON_DESCRIPTOR = msg({
	message: "You can't grant a permission you don't have in this channel.",
	comment:
		'Inline tooltip on a disabled permission checkbox when the editor would be granting a permission they themselves do not hold in this channel.',
});
const YOU_CAN_T_REMOVE_THIS_PERMISSION_BECAUSE_IT_DESCRIPTOR = msg({
	message: "You can't remove this permission because it would remove it from you.",
	comment:
		'Inline tooltip on a disabled permission checkbox when removing it would lock the editor out of further permission changes.',
});
const UNKNOWN_ROLE_DESCRIPTOR = msg({
	message: 'Unknown role',
	comment: 'Fallback label in channel permissions when a role override references a role that is not loaded.',
});
const UNKNOWN_USER_DESCRIPTOR = msg({
	message: 'Unknown user',
	comment: 'Fallback label in channel permissions when a member override references a user that is not loaded.',
});
const ChannelPermissionsTab: React.FC<{channelId: string}> = observer(({channelId}) => {
	const {i18n} = useLingui();
	const channel = Channels.getChannel(channelId);
	const parentChannel = Channels.getChannel(channel?.parentId || '');
	const guild = Guilds.getGuild(channel?.guildId || '');
	const currentUser = Users.currentUser;
	const isMobile = MobileLayout.enabled;
	const overrideOwnerId = useMemo(() => `channel-permissions-${channelId}`, [channelId]);
	const [selectedOverwriteId, setSelectedOverwriteId] = useState<string | null>(null);
	const [mobileShowEditor, setMobileShowEditor] = useState(false);
	const [overwriteUpdates, setOverwriteUpdates] = useState<Map<string, PermissionOverwrite>>(new Map());
	const [deletedOverwriteIds, setDeletedOverwriteIds] = useState<Set<string>>(new Set());
	const [newOverwriteIds, setNewOverwriteIds] = useState<Set<string>>(new Set());
	const [permissionSearchQuery, setPermissionSearchQuery] = useState('');
	const [isAddOverrideOpen, setIsAddOverrideOpen] = useState(false);
	const {
		refs: addOverrideRefs,
		floatingStyles: addOverrideFloatingStyles,
		context: addOverrideContext,
	} = useFloating({
		open: isAddOverrideOpen,
		onOpenChange: setIsAddOverrideOpen,
		placement: 'bottom-start',
		middleware: [offset(8), flip(), shift({padding: 8})],
	});
	const {getReferenceProps: getAddOverrideReferenceProps, getFloatingProps: getAddOverrideFloatingProps} =
		useInteractions([
			useClick(addOverrideContext),
			useDismiss(addOverrideContext, {escapeKey: false}),
			useRole(addOverrideContext),
		]);
	const canManageChannels = Permission.can(Permissions.MANAGE_CHANNELS, {guildId: channel?.guildId || ''});
	const canManageRoles = Permission.can(Permissions.MANAGE_ROLES, {guildId: channel?.guildId || ''});
	const manageChannelsPermissionLabel = PermissionUtils.formatPermissionLabel(i18n, Permissions.MANAGE_CHANNELS);
	const manageRolesPermissionLabel = PermissionUtils.formatPermissionLabel(i18n, Permissions.MANAGE_ROLES);
	useEffect(() => {
		const key = `channel-permissions-add-override-${channelId}`;
		if (isAddOverrideOpen) {
			LayerManager.addLayer('popout', key, () => setIsAddOverrideOpen(false));
		}
		return () => {
			LayerManager.removeLayer('popout', key);
		};
	}, [isAddOverrideOpen, channelId]);
	const currentUserPermissions = useMemo(() => {
		if (!guild || !currentUser || !channel) return 0n;
		return PermissionUtils.computePermissions(currentUser.id, channel.toJSON());
	}, [guild, currentUser, channel]);
	const currentUserMember = useMemo(() => {
		if (!guild || !currentUser) return null;
		return GuildMembers.getMember(guild.id, currentUser.id);
	}, [guild, currentUser]);
	const wouldRemoveOwnPermission = useCallback(
		(permission: bigint, overwriteId: string, overwriteType: 0 | 1): boolean => {
			if (!guild || !currentUser || !channel || !currentUserMember) return false;
			if (guild.isOwner(currentUser.id)) return false;
			const isEveryoneOverwrite = overwriteId === guild.id;
			const isMemberOverwrite = overwriteType === 1 && overwriteId === currentUser.id;
			const isRoleOverwrite = overwriteType === 0 && currentUserMember.roles.has(overwriteId);
			if (!isEveryoneOverwrite && !isMemberOverwrite && !isRoleOverwrite) return false;
			const currentOverwrite = channel.permissionOverwrites[overwriteId];
			if (!currentOverwrite) return false;
			const hasAllowForPermission = (currentOverwrite.allow & permission) === permission;
			if (!hasAllowForPermission) return false;
			const modifiedOverwrites: Record<string, {id: string; type: 0 | 1; allow: bigint; deny: bigint}> = {};
			for (const [id, ow] of Object.entries(channel.permissionOverwrites)) {
				if (id === overwriteId) {
					modifiedOverwrites[id] = {
						id,
						type: ow.type as 0 | 1,
						allow: ow.allow & ~permission,
						deny: ow.deny,
					};
				} else {
					modifiedOverwrites[id] = {
						id,
						type: ow.type as 0 | 1,
						allow: ow.allow,
						deny: ow.deny,
					};
				}
			}
			const permissionsWithoutAllow = PermissionUtils.computePermissions(
				currentUser.id,
				channel.toJSON(),
				modifiedOverwrites,
			);
			return (permissionsWithoutAllow & permission) !== permission;
		},
		[guild, currentUser, channel, currentUserMember],
	);
	const overwrites = useMemo(() => {
		if (!channel || !guild) return [];
		const everyoneOverwrite = channel.permissionOverwrites[guild.id];
		const otherOverwrites = Object.entries(channel.permissionOverwrites)
			.filter(([id]) => id !== guild.id && !deletedOverwriteIds.has(id))
			.map(([id, ow]) => ({
				id,
				type: ow.type as 0 | 1,
				allow: ow.allow,
				deny: ow.deny,
			}));
		for (const newId of newOverwriteIds) {
			if (!otherOverwrites.find((ow) => ow.id === newId)) {
				const update = overwriteUpdates.get(newId);
				if (update) {
					otherOverwrites.push(update);
				}
			}
		}
		const roles: Array<PermissionOverwrite> = [];
		const members: Array<PermissionOverwrite> = [];
		for (const ow of otherOverwrites) {
			if (ow.type === 0) {
				roles.push(ow);
			} else {
				members.push(ow);
			}
		}
		roles.sort((a, b) => {
			const roleA = guild.roles[a.id];
			const roleB = guild.roles[b.id];
			if (!roleA || !roleB) return 0;
			if (roleA.position !== roleB.position) {
				return roleB.position - roleA.position;
			}
			return BigInt(a.id) < BigInt(b.id) ? -1 : 1;
		});
		members.sort((a, b) => (BigInt(a.id) < BigInt(b.id) ? -1 : 1));
		const result: Array<PermissionOverwrite> = [...roles, ...members];
		result.push(
			everyoneOverwrite
				? {
						id: guild.id,
						type: 0 as const,
						allow: everyoneOverwrite.allow,
						deny: everyoneOverwrite.deny,
					}
				: {
						id: guild.id,
						type: 0 as const,
						allow: 0n,
						deny: 0n,
					},
		);
		return result;
	}, [channel, guild, deletedOverwriteIds, newOverwriteIds, overwriteUpdates]);
	const overwritesWithUpdates = useMemo(() => {
		return overwrites.map((ow) => {
			const update = overwriteUpdates.get(ow.id);
			return update || ow;
		});
	}, [overwrites, overwriteUpdates]);
	const selectedOverwrite = useMemo(() => {
		if (!selectedOverwriteId) return null;
		return overwritesWithUpdates.find((ow) => ow.id === selectedOverwriteId) || null;
	}, [selectedOverwriteId, overwritesWithUpdates]);
	const getPermissionDisabledReason = useCallback(
		(permission: bigint): string | undefined => {
			if (!guild || !currentUser) return;
			if (!canManageChannels)
				return i18n._(YOU_NEED_THE_PERMISSION_TO_EDIT_THESE_PERMISSIONS_DESCRIPTOR, {manageChannelsPermissionLabel});
			if (!canManageRoles)
				return i18n._(YOU_NEED_THE_PERMISSION_TO_EDIT_THESE_PERMISSIONS_2_DESCRIPTOR, {manageRolesPermissionLabel});
			if (guild.isOwner(currentUser.id)) return;
			if ((currentUserPermissions & permission) !== permission) {
				return i18n._(YOU_CAN_T_GRANT_A_PERMISSION_YOU_DON_DESCRIPTOR);
			}
			if (selectedOverwrite && wouldRemoveOwnPermission(permission, selectedOverwrite.id, selectedOverwrite.type)) {
				return i18n._(YOU_CAN_T_REMOVE_THIS_PERMISSION_BECAUSE_IT_DESCRIPTOR);
			}
			return;
		},
		[
			guild,
			currentUser,
			currentUserPermissions,
			canManageChannels,
			canManageRoles,
			manageChannelsPermissionLabel,
			manageRolesPermissionLabel,
			selectedOverwrite,
			wouldRemoveOwnPermission,
		],
	);
	const getPermissionWarning = useCallback(
		(permission: bigint): string | undefined => {
			if (!guild || !currentUser || !selectedOverwrite) return;
			if (guild.isOwner(currentUser.id)) return;
			if (wouldRemoveOwnPermission(permission, selectedOverwrite.id, selectedOverwrite.type)) {
				return i18n._(YOU_CAN_T_REMOVE_THIS_PERMISSION_BECAUSE_IT_DESCRIPTOR);
			}
			return;
		},
		[guild, currentUser, selectedOverwrite, wouldRemoveOwnPermission],
	);
	useLayoutEffect(() => {
		if (overwrites.length > 0 && !selectedOverwriteId) {
			setSelectedOverwriteId(overwrites[0].id);
		}
	}, [overwrites, selectedOverwriteId]);
	useEffect(() => {
		if (!guild) {
			return;
		}
		const userOverwriteIds = overwrites.filter((ow) => ow.type === 1).map((ow) => ow.id);
		if (userOverwriteIds.length > 0) {
			GuildMembers.ensureMembersLoaded(guild.id, userOverwriteIds).catch((error) => {
				channelPermissionsTabLogger.error('Failed to ensure members', error);
			});
		}
	}, [guild, overwrites]);
	const hasUnsavedChanges = useMemo(
		() => overwriteUpdates.size > 0 || deletedOverwriteIds.size > 0 || newOverwriteIds.size > 0,
		[overwriteUpdates, deletedOverwriteIds, newOverwriteIds],
	);
	useEffect(() => {
		UnsavedChangesCommands.setUnsavedChanges(CHANNEL_PERMISSIONS_TAB_ID, hasUnsavedChanges);
	}, [hasUnsavedChanges]);
	const handleSave = useCallback(async () => {
		if (!channel || !canManageChannels || !canManageRoles) return;
		try {
			const updatedOverwrites = overwritesWithUpdates
				.filter((ow) => !deletedOverwriteIds.has(ow.id))
				.filter((ow) => {
					if (ow.id === guild?.id && ow.allow === 0n && ow.deny === 0n) {
						return false;
					}
					return true;
				})
				.map((ow): {id: string; type: 0 | 1; allow: string; deny: string} => ({
					id: ow.id,
					type: ow.type,
					allow: ow.allow.toString(),
					deny: ow.deny.toString(),
				}));
			await ChannelCommands.updatePermissionOverwrites(channel.id, updatedOverwrites);
			setOverwriteUpdates(new Map());
			setDeletedOverwriteIds(new Set());
			setNewOverwriteIds(new Set());
			ToastCommands.createToast({
				type: 'success',
				children: <Trans>Channel access updated</Trans>,
			});
		} catch (_error) {
			ModalCommands.push(
				modal(() => (
					<ChannelPermissionsUpdateFailedModal data-flx="channel.channel-tabs.channel-permissions-tab.handle-save.channel-permissions-update-failed-modal" />
				)),
			);
		}
	}, [channel, canManageChannels, canManageRoles, overwritesWithUpdates, guild, deletedOverwriteIds]);
	const handleReset = useCallback(() => {
		setOverwriteUpdates(new Map());
		setDeletedOverwriteIds(new Set());
		setNewOverwriteIds(new Set());
		if (guild) {
			setSelectedOverwriteId(guild.id);
		}
	}, [guild]);
	useEffect(() => {
		UnsavedChangesCommands.setTabData(CHANNEL_PERMISSIONS_TAB_ID, {
			onReset: handleReset,
			onSave: handleSave,
			isSubmitting: false,
		});
	}, [handleReset, handleSave]);
	useEffect(() => {
		return () => {
			UnsavedChangesCommands.clearUnsavedChanges(CHANNEL_PERMISSIONS_TAB_ID);
		};
	}, []);
	const handleOverwriteUpdate = useCallback(
		(overwriteId: string, updates: Partial<PermissionOverwrite>) => {
			setOverwriteUpdates((prev) => {
				const newMap = new Map(prev);
				const current = newMap.get(overwriteId) || overwrites.find((o) => o.id === overwriteId);
				if (!current) return prev;
				const merged = {...current, ...updates};
				const originalOverwrite = channel?.permissionOverwrites[overwriteId];
				const isNewOverwrite = newOverwriteIds.has(overwriteId);
				if (isNewOverwrite) {
					newMap.set(overwriteId, merged);
					return newMap;
				}
				if (originalOverwrite) {
					const hasChanges = merged.allow !== originalOverwrite.allow || merged.deny !== originalOverwrite.deny;
					if (hasChanges) {
						newMap.set(overwriteId, merged);
					} else {
						newMap.delete(overwriteId);
					}
				} else {
					const hasPermissions = merged.allow !== 0n || merged.deny !== 0n;
					if (hasPermissions) {
						newMap.set(overwriteId, merged);
					} else {
						newMap.delete(overwriteId);
					}
				}
				return newMap;
			});
		},
		[overwrites, channel, newOverwriteIds],
	);
	const applyPermissionState = useCallback(
		(permissions: Array<bigint>, state: PermissionState, baseAllow: bigint, baseDeny: bigint) => {
			let newAllow = baseAllow;
			let newDeny = baseDeny;
			for (const permission of permissions) {
				if (state === 'ALLOW') {
					newAllow |= permission;
					newDeny &= ~permission;
				} else if (state === 'DENY') {
					newDeny |= permission;
					newAllow &= ~permission;
				} else {
					newAllow &= ~permission;
					newDeny &= ~permission;
				}
			}
			return {allow: newAllow, deny: newDeny};
		},
		[],
	);
	const handlePermissionChange = useCallback(
		(permission: bigint, state: PermissionState) => {
			if (!selectedOverwrite) return;
			const {allow, deny} = applyPermissionState([permission], state, selectedOverwrite.allow, selectedOverwrite.deny);
			handleOverwriteUpdate(selectedOverwrite.id, {allow, deny});
		},
		[selectedOverwrite, handleOverwriteUpdate, applyPermissionState],
	);
	const permissionSpecs = useMemo(() => {
		if (!channel) return [];
		return PermissionUtils.generateChannelPermissionSpecs(i18n, channel.type);
	}, [channel, i18n.locale]);
	const filteredPermissionSpecs = useMemo(() => {
		if (!permissionSearchQuery) return permissionSpecs;
		return permissionSpecs
			.map((spec) => {
				const filteredPermissions = matchSorter(spec.permissions, permissionSearchQuery, {
					keys: ['title', 'description'],
				});
				if (filteredPermissions.length === 0) return null;
				return {
					...spec,
					permissions: filteredPermissions,
				};
			})
			.filter((spec): spec is PermissionUtils.PermissionSpec => spec !== null);
	}, [permissionSpecs, permissionSearchQuery]);
	const allPermissionsState = useMemo(() => {
		if (!selectedOverwrite || filteredPermissionSpecs.length === 0) return;
		const allPermissions = filteredPermissionSpecs.flatMap((spec) => spec.permissions.map((p) => p.flag));
		if (allPermissions.length === 0) return;
		const firstState = getPermissionState(allPermissions[0], selectedOverwrite.allow, selectedOverwrite.deny);
		const allSameState = allPermissions.every(
			(perm) => getPermissionState(perm, selectedOverwrite.allow, selectedOverwrite.deny) === firstState,
		);
		return allSameState ? firstState : undefined;
	}, [selectedOverwrite, filteredPermissionSpecs]);
	const handleSetAllPermissions = useCallback(
		(state: PermissionState) => {
			if (!selectedOverwrite || !guild || !currentUser) return;
			const isOwner = guild.isOwner(currentUser.id);
			const allPermissions = permissionSpecs.flatMap((spec) => spec.permissions.map((p) => p.flag));
			const allowedPermissions = allPermissions.filter((perm) => {
				if (isOwner) return true;
				return (currentUserPermissions & perm) === perm;
			});
			const {allow, deny} = applyPermissionState(
				allowedPermissions,
				state,
				selectedOverwrite.allow,
				selectedOverwrite.deny,
			);
			handleOverwriteUpdate(selectedOverwrite.id, {allow, deny});
		},
		[
			selectedOverwrite,
			permissionSpecs,
			handleOverwriteUpdate,
			applyPermissionState,
			guild,
			currentUser,
			currentUserPermissions,
		],
	);
	const isSyncedWithParent = useMemo(() => {
		if (!channel || !channel.parentId || !parentChannel || !parentChannel.isGuildCategory()) return null;
		const channelOverwrites = channel.permissionOverwrites || {};
		const parentOverwrites = parentChannel.permissionOverwrites || {};
		const channelKeys = Object.keys(channelOverwrites).sort();
		const parentKeys = Object.keys(parentOverwrites).sort();
		if (channelKeys.length !== parentKeys.length) return false;
		if (channelKeys.join(',') !== parentKeys.join(',')) return false;
		for (const key of channelKeys) {
			const channelOw = channelOverwrites[key];
			const parentOw = parentOverwrites[key];
			if (!parentOw) return false;
			if (channelOw.type !== parentOw.type) return false;
			if (channelOw.allow !== parentOw.allow) return false;
			if (channelOw.deny !== parentOw.deny) return false;
		}
		return true;
	}, [channel, parentChannel]);
	const handleSyncWithParent = useCallback(async () => {
		if (!channel || !parentChannel || !canManageChannels || !canManageRoles) return;
		const parentOverwrites = parentChannel.permissionOverwrites || {};
		const updatedOverwrites = Object.entries(parentOverwrites).map(
			([id, ow]): {id: string; type: 0 | 1; allow: string; deny: string} => ({
				id,
				type: ow.type as 0 | 1,
				allow: ow.allow.toString(),
				deny: ow.deny.toString(),
			}),
		);
		try {
			await ChannelCommands.updatePermissionOverwrites(channel.id, updatedOverwrites);
			setOverwriteUpdates(new Map());
			setDeletedOverwriteIds(new Set());
			setNewOverwriteIds(new Set());
			ToastCommands.createToast({
				type: 'success',
				children: <Trans>Channel synced with parent category</Trans>,
			});
		} catch (_error) {
			ModalCommands.push(
				modal(() => (
					<ChannelPermissionsUpdateFailedModal data-flx="channel.channel-tabs.channel-permissions-tab.handle-sync-with-parent.channel-permissions-update-failed-modal" />
				)),
			);
		}
	}, [channel, parentChannel, canManageChannels, canManageRoles]);
	const handleAddOverride = useCallback((id: string, type: 0 | 1, _name: string) => {
		const newOverwrite: PermissionOverwrite = {
			id,
			type,
			allow: 0n,
			deny: 0n,
		};
		setNewOverwriteIds((prev) => new Set(prev).add(id));
		setOverwriteUpdates((prev) => {
			const newMap = new Map(prev);
			newMap.set(id, newOverwrite);
			return newMap;
		});
		setSelectedOverwriteId(id);
	}, []);
	const removeOverwrite = useCallback(
		(overwriteId: string) => {
			if (!guild || overwriteId === guild.id) return;
			const futureOverwrites = overwritesWithUpdates.filter((o) => o.id !== overwriteId);
			const currentIndex = overwritesWithUpdates.findIndex((o) => o.id === overwriteId);
			const nextOverwrite = futureOverwrites[currentIndex] ?? futureOverwrites[0];
			if (newOverwriteIds.has(overwriteId)) {
				setNewOverwriteIds((prev) => {
					const newSet = new Set(prev);
					newSet.delete(overwriteId);
					return newSet;
				});
			} else {
				setDeletedOverwriteIds((prev) => new Set(prev).add(overwriteId));
			}
			setOverwriteUpdates((prev) => {
				const newMap = new Map(prev);
				newMap.delete(overwriteId);
				return newMap;
			});
			setSelectedOverwriteId((prev) => (prev === overwriteId ? (nextOverwrite?.id ?? null) : prev));
		},
		[guild, overwritesWithUpdates, newOverwriteIds],
	);
	const handleDeleteOverride = useCallback(() => {
		if (!selectedOverwrite) return;
		removeOverwrite(selectedOverwrite.id);
	}, [selectedOverwrite, removeOverwrite]);
	const getOverwriteName = useCallback(
		(overwrite: PermissionOverwrite): string => {
			if (!guild) return '';
			if (overwrite.id === guild.id) return '@everyone';
			if (overwrite.type === 0) {
				const role = guild.roles[overwrite.id];
				return role?.name || i18n._(UNKNOWN_ROLE_DESCRIPTOR);
			}
			const user = Users.getUser(overwrite.id);
			return user ? NicknameUtils.getDisplayName(user) : i18n._(UNKNOWN_USER_DESCRIPTOR);
		},
		[guild, i18n],
	);
	const getOverwriteColor = useCallback(
		(overwrite: PermissionOverwrite): number | undefined => {
			if (!guild || overwrite.type !== 0) return;
			const role = guild.roles[overwrite.id];
			return role?.color;
		},
		[guild],
	);
	const getOverwriteUser = useCallback((overwrite: PermissionOverwrite): User | null => {
		if (overwrite.type !== 1) return null;
		return Users.getUser(overwrite.id) || null;
	}, []);
	const handleMobileOverwriteSelect = useCallback((overwriteId: string) => {
		setSelectedOverwriteId(overwriteId);
		setMobileShowEditor(true);
	}, []);
	const handleMobileBack = useCallback(() => {
		setMobileShowEditor(false);
	}, []);
	const existingOverwriteIds = useMemo(() => {
		return new Set(overwritesWithUpdates.map((o) => o.id));
	}, [overwritesWithUpdates]);
	const isEveryoneSelected = selectedOverwrite?.id === guild?.id;
	const sidebarContent = useMemo(() => {
		if (!channel || !guild || !currentUser) return null;
		return (
			<ChannelPermissionsSidebar
				guildId={guild.id}
				overwritesWithUpdates={overwritesWithUpdates}
				selectedOverwriteId={selectedOverwriteId}
				canManageChannels={canManageChannels}
				canManageRoles={canManageRoles}
				canManageOverwrites={canManageChannels && canManageRoles}
				onDeleteOverwrite={removeOverwrite}
				isAddOverrideOpen={isAddOverrideOpen}
				setIsAddOverrideOpen={setIsAddOverrideOpen}
				existingOverwriteIds={existingOverwriteIds}
				addOverrideContext={addOverrideContext}
				addOverrideFloatingStyles={addOverrideFloatingStyles}
				addOverrideReferenceRef={addOverrideRefs.setReference}
				addOverrideFloatingRef={addOverrideRefs.setFloating}
				addOverrideReferenceWidth={
					addOverrideRefs.reference.current instanceof HTMLElement
						? addOverrideRefs.reference.current.offsetWidth
						: undefined
				}
				getAddOverrideReferenceProps={getAddOverrideReferenceProps}
				getAddOverrideFloatingProps={getAddOverrideFloatingProps}
				onAddOverride={handleAddOverride}
				onSelectOverwrite={setSelectedOverwriteId}
				getOverwriteName={getOverwriteName}
				getOverwriteColor={getOverwriteColor}
				getOverwriteUser={getOverwriteUser}
				data-flx="channel.channel-tabs.channel-permissions-tab.sidebar-content.channel-permissions-sidebar"
			/>
		);
	}, [
		channel,
		guild,
		currentUser,
		selectedOverwriteId,
		overwritesWithUpdates,
		canManageChannels,
		canManageRoles,
		removeOverwrite,
		isAddOverrideOpen,
		addOverrideContext,
		addOverrideFloatingStyles,
		getAddOverrideFloatingProps,
		getAddOverrideReferenceProps,
		handleAddOverride,
		getOverwriteName,
		getOverwriteColor,
		getOverwriteUser,
		addOverrideRefs.reference,
		addOverrideRefs.setReference,
		addOverrideRefs.setFloating,
		existingOverwriteIds,
	]);
	useEffect(() => {
		if (isMobile || !channel || !guild || !currentUser) return;
		if (!SettingsSidebar.hasOverride) {
			SettingsSidebar.setOverride(overrideOwnerId, sidebarContent, {defaultOn: true});
		}
		return () => {
			if (SettingsSidebar.ownerId === overrideOwnerId) {
				SettingsSidebar.clearOverride(overrideOwnerId);
			}
		};
	}, [overrideOwnerId, isMobile]);
	useEffect(() => {
		if (isMobile || !channel || !guild || !currentUser) return;
		if (SettingsSidebar.ownerId === overrideOwnerId && sidebarContent) {
			SettingsSidebar.updateOverride(overrideOwnerId, sidebarContent);
		}
	}, [sidebarContent, channel, guild, currentUser, overrideOwnerId, isMobile]);
	const isComfyLayout = PermissionLayout.isComfy;
	const isGridLayout = PermissionLayout.isGrid;
	const permissionsScrollerKey = useMemo(
		() => `channel-permissions-right-scroller-${isComfyLayout ? 'comfy' : 'dense'}-${isGridLayout ? 'grid' : 'single'}`,
		[isComfyLayout, isGridLayout],
	);
	if (!channel || !guild || !currentUser) return null;
	if (isMobile && !mobileShowEditor) {
		return (
			<MobileOverrideListView
				guildId={guild.id}
				parentChannel={parentChannel}
				isSyncedWithParent={isSyncedWithParent}
				canManageChannels={canManageChannels}
				canManageRoles={canManageRoles}
				onSyncWithParent={handleSyncWithParent}
				overwritesWithUpdates={overwritesWithUpdates}
				isAddOverrideOpen={isAddOverrideOpen}
				setIsAddOverrideOpen={setIsAddOverrideOpen}
				addOverrideContext={addOverrideContext}
				addOverrideFloatingStyles={addOverrideFloatingStyles}
				addOverrideFloatingRef={addOverrideRefs.setFloating}
				getAddOverrideFloatingProps={getAddOverrideFloatingProps}
				existingOverwriteIds={existingOverwriteIds}
				onAddOverride={handleAddOverride}
				getOverwriteName={getOverwriteName}
				getOverwriteColor={getOverwriteColor}
				getOverwriteUser={getOverwriteUser}
				onMobileOverwriteSelect={handleMobileOverwriteSelect}
				data-flx="channel.channel-tabs.channel-permissions-tab.mobile-override-list-view"
			/>
		);
	}
	return (
		<div className={styles.container} data-flx="channel.channel-tabs.channel-permissions-tab.container--2">
			{isSyncedWithParent !== null && (
				<SyncWithParentBanner
					isSyncedWithParent={isSyncedWithParent}
					parentChannel={parentChannel}
					canManageChannels={canManageChannels}
					canManageRoles={canManageRoles}
					onSync={handleSyncWithParent}
					variant="desktop"
					data-flx="channel.channel-tabs.channel-permissions-tab.sync-with-parent-banner"
				/>
			)}
			<div className={styles.grid} data-flx="channel.channel-tabs.channel-permissions-tab.grid">
				<div className={styles.right} data-flx="channel.channel-tabs.channel-permissions-tab.right">
					<div
						className={styles.rightScroller}
						key={permissionsScrollerKey}
						data-flx="channel.channel-tabs.channel-permissions-tab.right-scroller"
					>
						{selectedOverwrite && (
							<PermissionEditorPanel
								selectedOverwrite={selectedOverwrite}
								isMobile={isMobile}
								isEveryoneSelected={isEveryoneSelected}
								canManageChannels={canManageChannels}
								canManageRoles={canManageRoles}
								onMobileBack={handleMobileBack}
								getOverwriteName={getOverwriteName}
								onDeleteOverride={handleDeleteOverride}
								allPermissionsState={allPermissionsState}
								onSetAllPermissions={handleSetAllPermissions}
								permissionSearchQuery={permissionSearchQuery}
								setPermissionSearchQuery={setPermissionSearchQuery}
								filteredPermissionSpecs={filteredPermissionSpecs}
								onPermissionChange={handlePermissionChange}
								getPermissionDisabledReason={getPermissionDisabledReason}
								getPermissionWarning={getPermissionWarning}
								data-flx="channel.channel-tabs.channel-permissions-tab.permission-editor-panel"
							/>
						)}
					</div>
				</div>
			</div>
		</div>
	);
});

export default ChannelPermissionsTab;
