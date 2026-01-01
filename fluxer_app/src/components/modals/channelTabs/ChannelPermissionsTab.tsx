/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {
	FloatingFocusManager,
	flip,
	offset,
	shift,
	useClick,
	useDismiss,
	useFloating,
	useInteractions,
	useRole,
} from '@floating-ui/react';
import {Trans, useLingui} from '@lingui/react/macro';
import {
	CaretRightIcon,
	GridFourIcon,
	ListIcon,
	MagnifyingGlassIcon,
	PlusIcon,
	RowsIcon,
	TrashIcon,
	UsersIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {matchSorter} from 'match-sorter';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ChannelActionCreators from '~/actions/ChannelActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as UnsavedChangesActionCreators from '~/actions/UnsavedChangesActionCreators';
import {Permissions} from '~/Constants';
import {ChannelPermissionsUpdateFailedModal} from '~/components/alerts/ChannelPermissionsUpdateFailedModal';
import {Input} from '~/components/form/Input';
import {Avatar} from '~/components/uikit/Avatar';
import {Button} from '~/components/uikit/Button/Button';
import {Scroller} from '~/components/uikit/Scroller';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import type {UserRecord} from '~/records/UserRecord';
import ChannelStore from '~/stores/ChannelStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildStore from '~/stores/GuildStore';
import LayerManager from '~/stores/LayerManager';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import PermissionLayoutStore from '~/stores/PermissionLayoutStore';
import PermissionStore from '~/stores/PermissionStore';
import SettingsSidebarStore from '~/stores/SettingsSidebarStore';
import UserStore from '~/stores/UserStore';
import * as PermissionUtils from '~/utils/PermissionUtils';
import {AddOverridePopout} from '../shared/AddOverridePopout';
import {
	DEFAULT_ROLE_COLOR_HEX,
	getPermissionState,
	getRoleColor,
	PermissionOverwriteCategory,
	type PermissionState,
	PermissionStateButtons,
} from '../shared/PermissionComponents';
import styles from './ChannelPermissionsTab.module.css';

interface PermissionOverwrite {
	id: string;
	type: 0 | 1;
	allow: bigint;
	deny: bigint;
}

const CHANNEL_PERMISSIONS_TAB_ID = 'permissions';

const OverwriteItem: React.FC<{
	overwrite: PermissionOverwrite;
	name: string;
	color?: number;
	user?: UserRecord | null;
	isSelected: boolean;
	isEveryone: boolean;
	onClick: () => void;
	guildId: string;
}> = observer(({overwrite, name, color, user, isSelected, isEveryone, onClick, guildId}) => {
	return (
		<button
			type="button"
			className={clsx(styles.overwriteItem, {[styles.overwriteItemSelected]: isSelected})}
			onClick={onClick}
		>
			{overwrite.type === 0 && !isEveryone ? (
				<div
					className={styles.roleDot}
					style={{backgroundColor: color === 0 ? DEFAULT_ROLE_COLOR_HEX : getRoleColor(color || 0)}}
				/>
			) : overwrite.type === 1 && user ? (
				<Avatar user={user} size={12} className={styles.overwriteIcon} guildId={guildId} />
			) : (
				<UsersIcon className={styles.overwriteIcon} />
			)}
			<span className={styles.overwriteName}>{name}</span>
		</button>
	);
});

const ChannelPermissionsTab: React.FC<{channelId: string}> = observer(({channelId}) => {
	const {t, i18n} = useLingui();
	const channel = ChannelStore.getChannel(channelId);
	const parentChannel = ChannelStore.getChannel(channel?.parentId || '');
	const guild = GuildStore.getGuild(channel?.guildId || '');
	const currentUser = UserStore.currentUser;
	const isMobile = MobileLayoutStore.enabled;

	const overrideOwnerId = React.useMemo(() => `channel-permissions-${channelId}`, [channelId]);

	const [selectedOverwriteId, setSelectedOverwriteId] = React.useState<string | null>(null);
	const [mobileShowEditor, setMobileShowEditor] = React.useState(false);
	const [overwriteUpdates, setOverwriteUpdates] = React.useState<Map<string, PermissionOverwrite>>(new Map());
	const [deletedOverwriteIds, setDeletedOverwriteIds] = React.useState<Set<string>>(new Set());
	const [newOverwriteIds, setNewOverwriteIds] = React.useState<Set<string>>(new Set());
	const [permissionSearchQuery, setPermissionSearchQuery] = React.useState('');

	const [isAddOverrideOpen, setIsAddOverrideOpen] = React.useState(false);
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

	const canManageChannels = PermissionStore.can(Permissions.MANAGE_CHANNELS, {guildId: channel?.guildId || ''});
	const canManageRoles = PermissionStore.can(Permissions.MANAGE_ROLES, {guildId: channel?.guildId || ''});

	React.useEffect(() => {
		const key = `channel-permissions-add-override-${channelId}`;
		if (isAddOverrideOpen) {
			LayerManager.addLayer('popout', key, () => setIsAddOverrideOpen(false));
		}
		return () => {
			LayerManager.removeLayer('popout', key);
		};
	}, [isAddOverrideOpen, channelId]);

	const currentUserPermissions = React.useMemo(() => {
		if (!guild || !currentUser || !channel) return 0n;
		return PermissionUtils.computePermissions(currentUser.id, channel.toJSON());
	}, [guild, currentUser, channel]);

	const currentUserMember = React.useMemo(() => {
		if (!guild || !currentUser) return null;
		return GuildMemberStore.getMember(guild.id, currentUser.id);
	}, [guild, currentUser]);

	const wouldRemoveOwnPermission = React.useCallback(
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

	const overwrites = React.useMemo(() => {
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

	const overwritesWithUpdates = React.useMemo(() => {
		return overwrites.map((ow) => {
			const update = overwriteUpdates.get(ow.id);
			return update || ow;
		});
	}, [overwrites, overwriteUpdates]);

	const selectedOverwrite = React.useMemo(() => {
		if (!selectedOverwriteId) return null;
		return overwritesWithUpdates.find((ow) => ow.id === selectedOverwriteId) || null;
	}, [selectedOverwriteId, overwritesWithUpdates]);

	const getPermissionDisabledReason = React.useCallback(
		(permission: bigint): string | undefined => {
			if (!guild || !currentUser) return;

			if (!canManageChannels) return t`You need Manage Channels to edit these permissions`;
			if (!canManageRoles) return t`You need Manage Roles to edit these permissions`;
			if (guild.isOwner(currentUser.id)) return;

			if ((currentUserPermissions & permission) !== permission) {
				return t`You cannot grant a permission you don't have in this channel`;
			}

			if (selectedOverwrite && wouldRemoveOwnPermission(permission, selectedOverwrite.id, selectedOverwrite.type)) {
				return t`You cannot remove this permission because it would remove it from yourself`;
			}

			return;
		},
		[
			guild,
			currentUser,
			currentUserPermissions,
			canManageChannels,
			canManageRoles,
			selectedOverwrite,
			wouldRemoveOwnPermission,
		],
	);

	const getPermissionWarning = React.useCallback(
		(permission: bigint): string | undefined => {
			if (!guild || !currentUser || !selectedOverwrite) return;
			if (guild.isOwner(currentUser.id)) return;
			if (wouldRemoveOwnPermission(permission, selectedOverwrite.id, selectedOverwrite.type)) {
				return t`You cannot remove this permission because it would remove it from yourself`;
			}
			return;
		},
		[guild, currentUser, selectedOverwrite, wouldRemoveOwnPermission],
	);

	React.useLayoutEffect(() => {
		if (overwrites.length > 0 && !selectedOverwriteId) {
			setSelectedOverwriteId(overwrites[0].id);
		}
	}, [overwrites, selectedOverwriteId]);

	React.useEffect(() => {
		if (!guild) {
			return;
		}
		const userOverwriteIds = overwrites.filter((ow) => ow.type === 1).map((ow) => ow.id);
		if (userOverwriteIds.length > 0) {
			GuildMemberStore.ensureMembersLoaded(guild.id, userOverwriteIds);
		}
	}, [guild, overwrites]);

	const hasUnsavedChanges = React.useMemo(
		() => overwriteUpdates.size > 0 || deletedOverwriteIds.size > 0 || newOverwriteIds.size > 0,
		[overwriteUpdates, deletedOverwriteIds, newOverwriteIds],
	);

	React.useEffect(() => {
		UnsavedChangesActionCreators.setUnsavedChanges(CHANNEL_PERMISSIONS_TAB_ID, hasUnsavedChanges);
	}, [hasUnsavedChanges]);

	const handleSave = React.useCallback(async () => {
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

			await ChannelActionCreators.updatePermissionOverwrites(channel.id, updatedOverwrites);
			setOverwriteUpdates(new Map());
			setDeletedOverwriteIds(new Set());
			setNewOverwriteIds(new Set());

			ToastActionCreators.createToast({
				type: 'success',
				children: <Trans>Channel access updated</Trans>,
			});
		} catch (_error) {
			ModalActionCreators.push(modal(() => <ChannelPermissionsUpdateFailedModal />));
		}
	}, [channel, canManageChannels, canManageRoles, overwritesWithUpdates, guild, deletedOverwriteIds]);

	const handleReset = React.useCallback(() => {
		setOverwriteUpdates(new Map());
		setDeletedOverwriteIds(new Set());
		setNewOverwriteIds(new Set());
		if (guild) {
			setSelectedOverwriteId(guild.id);
		}
	}, [guild]);

	React.useEffect(() => {
		UnsavedChangesActionCreators.setTabData(CHANNEL_PERMISSIONS_TAB_ID, {
			onReset: handleReset,
			onSave: handleSave,
			isSubmitting: false,
		});
	}, [handleReset, handleSave]);

	React.useEffect(() => {
		return () => {
			UnsavedChangesActionCreators.clearUnsavedChanges(CHANNEL_PERMISSIONS_TAB_ID);
		};
	}, []);

	const handleOverwriteUpdate = React.useCallback(
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

	const applyPermissionState = React.useCallback(
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

	const handlePermissionChange = React.useCallback(
		(permission: bigint, state: PermissionState) => {
			if (!selectedOverwrite) return;

			const {allow, deny} = applyPermissionState([permission], state, selectedOverwrite.allow, selectedOverwrite.deny);
			handleOverwriteUpdate(selectedOverwrite.id, {allow, deny});
		},
		[selectedOverwrite, handleOverwriteUpdate, applyPermissionState],
	);

	const permissionSpecs = React.useMemo(() => {
		if (!channel) return [];
		const specs = [
			PermissionUtils.generateChannelAccessPermissionSpec(i18n),
			PermissionUtils.generateChannelGeneralPermissionSpec(i18n),
		];

		if (channel.isVoice()) {
			specs.push(PermissionUtils.generateChannelVoicePermissionSpec(i18n));
		} else {
			specs.push(PermissionUtils.generateChannelTextPermissionSpec(i18n));
		}

		return specs;
	}, [channel, i18n]);

	const filteredPermissionSpecs = React.useMemo(() => {
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

	const allPermissionsState = React.useMemo(() => {
		if (!selectedOverwrite || filteredPermissionSpecs.length === 0) return;

		const allPermissions = filteredPermissionSpecs.flatMap((spec) => spec.permissions.map((p) => p.flag));
		if (allPermissions.length === 0) return;

		const firstState = getPermissionState(allPermissions[0], selectedOverwrite.allow, selectedOverwrite.deny);
		const allSameState = allPermissions.every(
			(perm) => getPermissionState(perm, selectedOverwrite.allow, selectedOverwrite.deny) === firstState,
		);

		return allSameState ? firstState : undefined;
	}, [selectedOverwrite, filteredPermissionSpecs]);

	const handleSetAllPermissions = React.useCallback(
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

	const isSyncedWithParent = React.useMemo(() => {
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

	const handleSyncWithParent = React.useCallback(async () => {
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
			await ChannelActionCreators.updatePermissionOverwrites(channel.id, updatedOverwrites);
			ToastActionCreators.createToast({
				type: 'success',
				children: <Trans>Channel synced with parent category</Trans>,
			});
		} catch (_error) {
			ModalActionCreators.push(modal(() => <ChannelPermissionsUpdateFailedModal />));
		}
	}, [channel, parentChannel, canManageChannels, canManageRoles]);

	const handleAddOverride = React.useCallback((id: string, type: 0 | 1, _name: string) => {
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

	const handleDeleteOverride = React.useCallback(() => {
		if (!selectedOverwrite || !guild || selectedOverwrite.id === guild.id) return;

		const futureOverwrites = overwritesWithUpdates.filter((o) => o.id !== selectedOverwrite.id);
		const currentIndex = overwritesWithUpdates.findIndex((o) => o.id === selectedOverwrite.id);

		const nextOverwrite = futureOverwrites[currentIndex] ?? futureOverwrites[0];

		if (newOverwriteIds.has(selectedOverwrite.id)) {
			setNewOverwriteIds((prev) => {
				const newSet = new Set(prev);
				newSet.delete(selectedOverwrite.id);
				return newSet;
			});
		} else {
			setDeletedOverwriteIds((prev) => new Set(prev).add(selectedOverwrite.id));
		}

		setOverwriteUpdates((prev) => {
			const newMap = new Map(prev);
			newMap.delete(selectedOverwrite.id);
			return newMap;
		});
		setSelectedOverwriteId(nextOverwrite?.id ?? null);
	}, [selectedOverwrite, guild, overwritesWithUpdates, newOverwriteIds]);

	const getOverwriteName = React.useCallback(
		(overwrite: PermissionOverwrite): string => {
			if (!guild) return '';
			if (overwrite.id === guild.id) return '@everyone';
			if (overwrite.type === 0) {
				const role = guild.roles[overwrite.id];
				return role?.name || 'Unknown Role';
			}
			const user = UserStore.getUser(overwrite.id);
			return user?.username || 'Unknown Member';
		},
		[guild],
	);

	const getOverwriteColor = React.useCallback(
		(overwrite: PermissionOverwrite): number | undefined => {
			if (!guild || overwrite.type !== 0) return;
			const role = guild.roles[overwrite.id];
			return role?.color;
		},
		[guild],
	);

	const getOverwriteUser = React.useCallback((overwrite: PermissionOverwrite): UserRecord | null => {
		if (overwrite.type !== 1) return null;
		return UserStore.getUser(overwrite.id) || null;
	}, []);

	const handleMobileOverwriteSelect = React.useCallback((overwriteId: string) => {
		setSelectedOverwriteId(overwriteId);
		setMobileShowEditor(true);
	}, []);

	const handleMobileBack = React.useCallback(() => {
		setMobileShowEditor(false);
	}, []);

	const existingOverwriteIds = React.useMemo(() => {
		return new Set(overwritesWithUpdates.map((o) => o.id));
	}, [overwritesWithUpdates]);

	const isEveryoneSelected = selectedOverwrite?.id === guild?.id;

	const sidebarContent = React.useMemo(() => {
		if (!channel || !guild || !currentUser) return null;

		return (
			<div>
				<div className={styles.leftTitle} style={{padding: '6px 8px'}}>
					<Trans>Access Overrides</Trans>
				</div>
				<div ref={addOverrideRefs.setReference} {...getAddOverrideReferenceProps()} style={{padding: '0 8px 8px 8px'}}>
					<Button
						variant="secondary"
						small={true}
						disabled={!canManageChannels || !canManageRoles}
						leftIcon={<PlusIcon size={18} weight="bold" />}
						onClick={() => setIsAddOverrideOpen(!isAddOverrideOpen)}
					>
						<Trans>Add Override</Trans>
					</Button>
				</div>
				{isAddOverrideOpen && (
					<FloatingFocusManager context={addOverrideContext} modal={true}>
						<div
							ref={addOverrideRefs.setFloating}
							style={{
								...addOverrideFloatingStyles,
								zIndex: 99999,
								width: (addOverrideRefs.reference.current as any)?.offsetWidth || 'auto',
								left: addOverrideFloatingStyles.left,
							}}
							{...getAddOverrideFloatingProps()}
						>
							<AddOverridePopout
								guildId={guild.id}
								existingOverwriteIds={existingOverwriteIds}
								onSelect={handleAddOverride}
								onClose={() => setIsAddOverrideOpen(false)}
								context={addOverrideContext}
							/>
						</div>
					</FloatingFocusManager>
				)}

				<div style={{padding: '0 8px 8px 8px'}}>
					{overwritesWithUpdates.map((overwrite) => {
						const name = getOverwriteName(overwrite);
						const color = getOverwriteColor(overwrite);
						const user = getOverwriteUser(overwrite);
						const isSelected = selectedOverwriteId === overwrite.id;
						const isEveryone = overwrite.id === guild.id;

						return (
							<OverwriteItem
								key={overwrite.id}
								overwrite={overwrite}
								name={name}
								color={color}
								user={user}
								isSelected={isSelected}
								isEveryone={isEveryone}
								onClick={() => setSelectedOverwriteId(overwrite.id)}
								guildId={guild.id}
							/>
						);
					})}
				</div>
			</div>
		);
	}, [
		channel,
		guild,
		currentUser,
		selectedOverwriteId,
		overwritesWithUpdates,
		canManageChannels,
		canManageRoles,
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
		existingOverwriteIds,
	]);

	React.useEffect(() => {
		if (isMobile || !channel || !guild || !currentUser) return;
		if (!SettingsSidebarStore.hasOverride) {
			SettingsSidebarStore.setOverride(overrideOwnerId, sidebarContent, {defaultOn: true});
		}
		return () => {
			if (SettingsSidebarStore.ownerId === overrideOwnerId) {
				SettingsSidebarStore.clearOverride(overrideOwnerId);
			}
		};
	}, [overrideOwnerId, isMobile]);

	React.useEffect(() => {
		if (isMobile || !channel || !guild || !currentUser) return;
		if (SettingsSidebarStore.ownerId === overrideOwnerId && sidebarContent) {
			SettingsSidebarStore.updateOverride(overrideOwnerId, sidebarContent);
		}
	}, [sidebarContent, channel, guild, currentUser, overrideOwnerId, isMobile]);

	const isComfyLayout = PermissionLayoutStore.isComfy;
	const isGridLayout = PermissionLayoutStore.isGrid;
	const permissionsScrollerKey = React.useMemo(
		() => `channel-permissions-right-scroller-${isComfyLayout ? 'comfy' : 'dense'}-${isGridLayout ? 'grid' : 'single'}`,
		[isComfyLayout, isGridLayout],
	);

	if (!channel || !guild || !currentUser) return null;

	if (isMobile && !mobileShowEditor) {
		return (
			<div className={styles.container}>
				{isSyncedWithParent !== null && (
					<div
						className={clsx(
							styles.syncBanner,
							isSyncedWithParent ? styles.syncBannerSynced : styles.syncBannerUnsynced,
						)}
					>
						<div>
							{isSyncedWithParent ? (
								<Trans>
									This channel is synced with the parent category <strong>{parentChannel?.name}</strong>.
								</Trans>
							) : (
								<Trans>
									This channel is not synced with the parent category <strong>{parentChannel?.name}</strong>.
								</Trans>
							)}
						</div>
						{!isSyncedWithParent && (
							<Button small={true} onClick={handleSyncWithParent} disabled={!canManageChannels || !canManageRoles}>
								<Trans>Sync with Category</Trans>
							</Button>
						)}
					</div>
				)}
				<div className={styles.mobileOverrideList}>
					<div className={styles.mobileListHeader}>
						<h2 className={styles.mobileListTitle}>
							<Trans>Access Overrides</Trans>
						</h2>
						<Button
							variant="secondary"
							small={true}
							leftIcon={<PlusIcon size={18} weight="bold" />}
							onClick={() => setIsAddOverrideOpen(!isAddOverrideOpen)}
							disabled={!canManageChannels || !canManageRoles}
						>
							<Trans>Add Override</Trans>
						</Button>
					</div>
					{isAddOverrideOpen && (
						<FloatingFocusManager context={addOverrideContext} modal={true}>
							<div
								ref={addOverrideRefs.setFloating}
								style={{
									...addOverrideFloatingStyles,
									zIndex: 99999,
								}}
								{...getAddOverrideFloatingProps()}
							>
								<AddOverridePopout
									guildId={guild.id}
									existingOverwriteIds={existingOverwriteIds}
									onSelect={handleAddOverride}
									onClose={() => setIsAddOverrideOpen(false)}
									context={addOverrideContext}
								/>
							</div>
						</FloatingFocusManager>
					)}
					<div className={styles.mobileOverrides}>
						{overwritesWithUpdates.map((overwrite) => {
							const name = getOverwriteName(overwrite);
							const color = getOverwriteColor(overwrite);
							const user = getOverwriteUser(overwrite);
							const isEveryone = overwrite.id === guild.id;

							return (
								<button
									key={overwrite.id}
									type="button"
									className={styles.mobileOverrideItem}
									onClick={() => handleMobileOverwriteSelect(overwrite.id)}
								>
									{overwrite.type === 0 && !isEveryone ? (
										<div
											className={styles.roleDot}
											style={{
												backgroundColor: color === 0 ? DEFAULT_ROLE_COLOR_HEX : getRoleColor(color || 0),
											}}
										/>
									) : overwrite.type === 1 && user ? (
										<Avatar user={user} size={12} className={styles.overwriteIcon} guildId={guild.id} />
									) : (
										<UsersIcon className={styles.overwriteIcon} />
									)}
									<span className={styles.mobileOverrideName}>{name}</span>
									<CaretRightIcon className={styles.mobileOverrideChevron} size={20} weight="bold" />
								</button>
							);
						})}
					</div>
				</div>
			</div>
		);
	}

	return (
		<div className={styles.container}>
			{isSyncedWithParent !== null && (
				<div
					className={clsx(styles.syncBanner, isSyncedWithParent ? styles.syncBannerSynced : styles.syncBannerUnsynced)}
				>
					<div>
						{isSyncedWithParent ? (
							<Trans>
								This channel is synced with the parent category <strong>{parentChannel?.name}</strong>.
							</Trans>
						) : (
							<Trans>
								This channel is not synced with the parent category <strong>{parentChannel?.name}</strong>.
							</Trans>
						)}
					</div>
					{!isSyncedWithParent && (
						<Button small={true} onClick={handleSyncWithParent} disabled={!canManageChannels || !canManageRoles}>
							<Trans>Sync with Category</Trans>
						</Button>
					)}
				</div>
			)}
			<div className={styles.grid}>
				<div className={styles.right}>
					<Scroller className={styles.rightScroller} fade={true} key={permissionsScrollerKey}>
						{selectedOverwrite && (
							<>
								{isMobile && (
									<div className={styles.mobileBackRow}>
										<Button variant="secondary" small={true} onClick={handleMobileBack}>
											<Trans>Back to Overrides</Trans>
										</Button>
									</div>
								)}
								<div className={styles.sectionRow}>
									<div className={styles.sectionHeader}>
										<h2 className={styles.sectionTitle}>
											<Trans>Edit Access for {getOverwriteName(selectedOverwrite)}</Trans>
										</h2>
										<p className={styles.subtleText}>
											{isEveryoneSelected ? (
												<Trans>Configure base access for this channel</Trans>
											) : selectedOverwrite?.type === 0 ? (
												<Trans>Configure overrides for this role</Trans>
											) : (
												<Trans>Configure overrides for this member</Trans>
											)}
										</p>
									</div>
									{!isEveryoneSelected && (
										<Button
											variant="secondary"
											small={true}
											leftIcon={<TrashIcon size={18} />}
											onClick={handleDeleteOverride}
											disabled={!canManageChannels || !canManageRoles}
										>
											<Trans>Remove Override</Trans>
										</Button>
									)}
								</div>

								<div className={styles.sectionRow}>
									<div className={styles.permHeaderRow}>
										<p className={styles.permHelp}>
											<Trans>Use these buttons to quickly set all permissions.</Trans>
										</p>
										<PermissionStateButtons
											currentState={allPermissionsState}
											onStateChange={handleSetAllPermissions}
											disabled={!canManageChannels || !canManageRoles}
										/>
									</div>
								</div>

								<div className={styles.permSearchRow}>
									<Input
										type="text"
										placeholder={t`Search Permissions...`}
										value={permissionSearchQuery}
										onChange={(e) => setPermissionSearchQuery(e.target.value)}
										leftIcon={<MagnifyingGlassIcon size={16} weight="bold" />}
										className={styles.permSearchInput}
									/>
									<div className={styles.layoutButtons}>
										<Tooltip text={PermissionLayoutStore.isComfy ? t`Dense layout` : t`Comfy layout`}>
											<button
												type="button"
												className={styles.layoutButton}
												onClick={() => PermissionLayoutStore.toggleLayoutMode()}
												aria-label={
													PermissionLayoutStore.isComfy ? t`Switch to dense layout` : t`Switch to comfy layout`
												}
											>
												{PermissionLayoutStore.isComfy ? (
													<RowsIcon size={20} weight="bold" />
												) : (
													<ListIcon size={20} weight="bold" />
												)}
											</button>
										</Tooltip>
										<Tooltip text={PermissionLayoutStore.isGrid ? t`Single column` : t`Two columns`}>
											<button
												type="button"
												className={styles.layoutButton}
												onClick={() => PermissionLayoutStore.toggleGridMode()}
												aria-label={
													PermissionLayoutStore.isGrid ? t`Switch to single column` : t`Switch to two columns`
												}
											>
												<GridFourIcon size={20} weight={PermissionLayoutStore.isGrid ? 'fill' : 'bold'} />
											</button>
										</Tooltip>
									</div>
								</div>

								<div className={styles.permCategories}>
									{filteredPermissionSpecs.map((spec, index) => (
										<PermissionOverwriteCategory
											key={spec.title}
											spec={spec}
											allow={selectedOverwrite.allow}
											deny={selectedOverwrite.deny}
											onPermissionChange={handlePermissionChange}
											disabled={!canManageChannels || !canManageRoles}
											getPermissionDisabledReason={getPermissionDisabledReason}
											getPermissionWarning={getPermissionWarning}
											isFirst={index === 0}
										/>
									))}
									{filteredPermissionSpecs.length === 0 && permissionSearchQuery && (
										<div className={styles.emptyState}>
											<Trans>No permissions found</Trans>
										</div>
									)}
								</div>
							</>
						)}
					</Scroller>
				</div>
			</div>
		</div>
	);
});

export default ChannelPermissionsTab;
