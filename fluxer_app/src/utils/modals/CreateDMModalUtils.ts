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

import {msg} from '@lingui/core/macro';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as NavigationActionCreators from '~/actions/NavigationActionCreators';
import * as PrivateChannelActionCreators from '~/actions/PrivateChannelActionCreators';
import {ME} from '~/Constants';
import i18n from '~/i18n';
import {Routes} from '~/Routes';
import {getDuplicateGroupDMChannels, MAX_GROUP_DM_RECIPIENTS} from '~/utils/groupDmUtils';
import * as RouterUtils from '~/utils/RouterUtils';

const arraysAreEqual = (left?: Array<string>, right?: Array<string>): boolean => {
	if (left === right) {
		return true;
	}
	if (!left || !right) {
		return false;
	}
	if (left.length !== right.length) {
		return false;
	}
	for (let i = 0; i < left.length; i += 1) {
		if (left[i] !== right[i]) {
			return false;
		}
	}
	return true;
};

export interface CreateDMModalProps {
	initialSelectedUserIds?: Array<string>;
	maxSelections?: number;
	duplicateExcludeChannelId?: string;
	autoCloseOnCreate?: boolean;
	resetKey?: unknown;
}

export interface CreateDMModalLogicState {
	selectedUserIds: Array<string>;
	isCreating: boolean;
	searchQuery: string;
	maxSelections: number;
	buttonText: string;
	subtitleText: string;
}

export interface CreateDMModalLogicActions {
	handleToggle: (userId: string) => void;
	handleCreate: () => Promise<{duplicates: Array<any>; selectionSnapshot: Array<string>} | null>;
	setSearchQuery: (query: string) => void;
	handleCreateChannel: (userIds: Array<string>) => Promise<void>;
}

export const useCreateDMModalLogic = (
	_props: CreateDMModalProps = {},
): CreateDMModalLogicState & CreateDMModalLogicActions => {
	const {
		initialSelectedUserIds,
		maxSelections: providedMaxSelections,
		duplicateExcludeChannelId,
		autoCloseOnCreate = true,
		resetKey,
	} = _props;
	const initialRecipients = React.useMemo(() => [...(initialSelectedUserIds ?? [])], [initialSelectedUserIds]);
	const [selectedUserIds, setSelectedUserIds] = React.useState<Array<string>>(() => initialRecipients);
	const initialRecipientsRef = React.useRef<Array<string>>(initialRecipients);
	const resetKeyRef = React.useRef<unknown>(resetKey);
	React.useEffect(() => {
		const recipientsChanged = !arraysAreEqual(initialRecipients, initialRecipientsRef.current);
		const resetKeyChanged = !Object.is(resetKey, resetKeyRef.current);
		if (recipientsChanged || resetKeyChanged) {
			initialRecipientsRef.current = initialRecipients;
			resetKeyRef.current = resetKey;
			setSelectedUserIds(initialRecipients);
		}
	}, [initialRecipients, resetKey]);
	const [isCreating, setIsCreating] = React.useState(false);
	const [searchQuery, setSearchQuery] = React.useState('');

	const maxSelections = providedMaxSelections ?? MAX_GROUP_DM_RECIPIENTS - 1;

	const handleToggle = React.useCallback((userId: string) => {
		setSelectedUserIds((prev) => {
			if (prev.includes(userId)) {
				return prev.filter((id) => id !== userId);
			}
			return [...prev, userId];
		});
	}, []);

	const createChannel = React.useCallback(async (userIds: Array<string>) => {
		setIsCreating(true);
		try {
			const channel =
				userIds.length === 0
					? await PrivateChannelActionCreators.createGroupDM([])
					: userIds.length === 1
						? await PrivateChannelActionCreators.create(userIds[0])
						: await PrivateChannelActionCreators.createGroupDM(userIds);

			if (autoCloseOnCreate) {
				ModalActionCreators.pop();
			}
			NavigationActionCreators.selectChannel(ME, channel.id);
			RouterUtils.transitionTo(Routes.dmChannel(channel.id));
		} catch (error) {
			console.error('Failed to create DM:', error);
		} finally {
			setIsCreating(false);
		}
	}, []);

	const handleCreate = React.useCallback(async () => {
		if (isCreating) return null;

		const selectionSnapshot = [...selectedUserIds];

		if (selectionSnapshot.length > 1) {
			const duplicates = getDuplicateGroupDMChannels(selectionSnapshot, duplicateExcludeChannelId);
			if (duplicates.length > 0) {
				return {duplicates, selectionSnapshot};
			}
		}

		await createChannel(selectionSnapshot);
		return null;
	}, [selectedUserIds, isCreating, createChannel, duplicateExcludeChannelId]);

	const buttonText = React.useMemo(() => {
		if (selectedUserIds.length === 0) {
			return i18n._(msg`Create Group DM`);
		}
		if (selectedUserIds.length === 1) {
			return i18n._(msg`Create DM`);
		}
		return i18n._(msg`Create Group DM`);
	}, [selectedUserIds.length]);

	const subtitleText = React.useMemo(() => {
		const remaining = Math.max(maxSelections - selectedUserIds.length, 0);
		if (selectedUserIds.length === 0) {
			return i18n._(msg`You can add up to ${maxSelections} friends`);
		}
		return i18n._(msg`You can add ${remaining} more friends`);
	}, [selectedUserIds.length, maxSelections]);

	return {
		selectedUserIds,
		isCreating,
		searchQuery,
		maxSelections,
		buttonText,
		subtitleText,
		handleToggle,
		handleCreate,
		setSearchQuery,
		handleCreateChannel: createChannel,
	};
};
