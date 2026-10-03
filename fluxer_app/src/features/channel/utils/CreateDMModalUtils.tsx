// SPDX-License-Identifier: AGPL-3.0-or-later

import i18n from '@app/app/I18n';
import {showDmActionErrorModal} from '@app/features/app/components/alerts/DmActionErrorModal';
import {StatusSlate} from '@app/features/app/components/dialogs/shared/StatusSlate';
import {openClaimAccountModal} from '@app/features/auth/components/modals/ClaimAccountModal';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import type {UnaddableRecipient} from '@app/features/channel/components/modals/UnaddableRecipientsConfirmModal';
import {UnaddableRecipientsConfirmModal} from '@app/features/channel/components/modals/UnaddableRecipientsConfirmModal';
import type {Channel} from '@app/features/channel/models/Channel';
import {getDuplicateGroupDMChannels, getMaxGroupDmOtherRecipients} from '@app/features/channel/utils/GroupDmUtils';
import {CLAIM_ACCOUNT_DESCRIPTOR, VERIFY_EMAIL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {focusChannelTextareaAfterNavigation} from '@app/features/messaging/utils/ChannelTextareaFocusUtils';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import {blockIfAccountLimited} from '@app/features/user/utils/AccountLimitUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ME} from '@fluxer/constants/src/AppConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {EnvelopeSimpleIcon, WarningCircleIcon} from '@phosphor-icons/react';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const CREATE_GROUP_DM_DESCRIPTOR = msg({
	message: 'Create group DM',
	comment: 'Button or menu action label in the create dm modal utils helper. Keep it concise.',
});
const CREATE_DM_DESCRIPTOR = msg({
	message: 'Create DM',
	comment: 'Button or menu action label in the create dm modal utils helper. Keep it concise.',
});
const CHOOSE_FRIENDS_TO_MESSAGE_DESCRIPTOR = msg({
	message: 'Choose friends to message.',
	comment: 'Subtitle in the create-DM modal. Keep it short and calm.',
});
const CLAIM_YOUR_ACCOUNT_DESCRIPTOR = msg({
	message: 'Claim your account',
	comment: 'Title shown when an unclaimed account cannot start a DM or group DM.',
});
const CLAIM_ACCOUNT_TO_START_DMS_DESCRIPTOR = msg({
	message: 'Claim your account to start DMs.',
	comment: 'Blocked-state text shown when an unclaimed account cannot start a DM or group DM.',
});
const VERIFY_YOUR_EMAIL_DESCRIPTOR = msg({
	message: 'Verify your email',
	comment: 'Title shown when an unverified account cannot start a DM or group DM.',
});
const VERIFY_EMAIL_TO_START_DMS_DESCRIPTOR = msg({
	message: 'Verify your email to start DMs.',
	comment: 'Blocked-state text shown when an unverified account cannot start a DM or group DM.',
});
export const DUPLICATE_GROUP_MODAL_KEY = 'create-dm-duplicate-group';
const UNADDABLE_RECIPIENTS_MODAL_KEY = 'create-dm-unaddable-recipients';

interface UnaddableRecipientsErrorPayload {
	code: typeof APIErrorCodes.GROUP_DM_RECIPIENTS_NOT_ADDABLE;
	unaddable_recipients: Array<{user_id: string; reason: UnaddableRecipient['reason']}>;
	addable_recipients: Array<string>;
}

function extractUnaddableRecipientsPayload(error: unknown): UnaddableRecipientsErrorPayload | null {
	if (!(error instanceof HttpError)) return null;
	const payload = error.body;
	if (
		payload &&
		typeof payload === 'object' &&
		(payload as {code?: unknown}).code === APIErrorCodes.GROUP_DM_RECIPIENTS_NOT_ADDABLE
	) {
		return payload as UnaddableRecipientsErrorPayload;
	}
	return null;
}

const logger = new Logger('CreateDMModalUtils');
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

export type CreateDmRestriction = 'unclaimed' | 'unverified';

export interface CreateDMModalLogicState {
	selectedUserIds: Array<string>;
	isCreating: boolean;
	searchQuery: string;
	maxSelections: number;
	buttonText: string;
	subtitleText: string;
	restriction: CreateDmRestriction | null;
}

export interface CreateDMModalLogicActions {
	handleToggle: (userId: string) => void;
	handleCreate: () => Promise<{
		duplicates: Array<Channel>;
		selectionSnapshot: Array<string>;
	} | null>;
	setSearchQuery: (query: string) => void;
	handleCreateChannel: (userIds: Array<string>) => Promise<void>;
}

export function getCreateDmRestriction(user: User | null | undefined): CreateDmRestriction | null {
	if (user && !user.isClaimed()) {
		return 'unclaimed';
	}
	if (user?.verified === false) {
		return 'unverified';
	}
	return null;
}

export function getCreateDMRestrictionMessage(i18nApi: I18n, restriction: CreateDmRestriction): string {
	if (restriction === 'unclaimed') {
		return i18nApi._(CLAIM_ACCOUNT_TO_START_DMS_DESCRIPTOR);
	}
	return i18nApi._(VERIFY_EMAIL_TO_START_DMS_DESCRIPTOR);
}

export const CreateDMRestrictionSlate: React.FC<
	{restriction: CreateDmRestriction; fullHeight?: boolean} & React.HTMLAttributes<HTMLDivElement>
> = ({restriction, fullHeight = true, ...props}) => {
	const {i18n: lingui} = useLingui();
	const isUnclaimed = restriction === 'unclaimed';
	return (
		<div data-flx="channel.create-dm-modal-utils.create-dm-restriction-slate.div" {...props}>
			<StatusSlate
				Icon={isUnclaimed ? WarningCircleIcon : EnvelopeSimpleIcon}
				title={lingui._(isUnclaimed ? CLAIM_YOUR_ACCOUNT_DESCRIPTOR : VERIFY_YOUR_EMAIL_DESCRIPTOR)}
				description={getCreateDMRestrictionMessage(lingui, restriction)}
				actions={[
					{
						text: lingui._(isUnclaimed ? CLAIM_ACCOUNT_DESCRIPTOR : VERIFY_EMAIL_DESCRIPTOR),
						onClick: () => {
							if (isUnclaimed) {
								openClaimAccountModal({force: true});
								return;
							}
							ModalCommands.push(
								modal(() => (
									<UserSettingsModal
										initialTab="account_security"
										data-flx="channel.create-dm-modal-utils.restriction-slate.user-settings-modal"
									/>
								)),
							);
						},
						variant: 'primary',
					},
				]}
				fullHeight={fullHeight}
				data-flx="channel.create-dm-modal-utils.create-dm-restriction-slate.status-slate"
			/>
		</div>
	);
};

export function useCreateDMModalLogic(
	_props: CreateDMModalProps = {},
): CreateDMModalLogicState & CreateDMModalLogicActions {
	const {
		initialSelectedUserIds,
		maxSelections: providedMaxSelections,
		duplicateExcludeChannelId,
		autoCloseOnCreate = true,
		resetKey,
	} = _props;
	const initialRecipients = useMemo(() => [...(initialSelectedUserIds ?? [])], [initialSelectedUserIds]);
	const [selectedUserIds, setSelectedUserIds] = useState<Array<string>>(() => initialRecipients);
	const initialRecipientsRef = useRef<Array<string>>(initialRecipients);
	const resetKeyRef = useRef<unknown>(resetKey);
	useEffect(() => {
		const recipientsChanged = !arraysAreEqual(initialRecipients, initialRecipientsRef.current);
		const resetKeyChanged = !Object.is(resetKey, resetKeyRef.current);
		if (recipientsChanged || resetKeyChanged) {
			initialRecipientsRef.current = initialRecipients;
			resetKeyRef.current = resetKey;
			setSelectedUserIds(initialRecipients);
		}
	}, [initialRecipients, resetKey]);
	const [isCreating, setIsCreating] = useState(false);
	const [searchQuery, setSearchQuery] = useState('');
	const maxSelections = providedMaxSelections ?? getMaxGroupDmOtherRecipients();
	const restriction = getCreateDmRestriction(Users.currentUser);
	const handleToggle = useCallback((userId: string) => {
		setSelectedUserIds((prev) => {
			if (prev.includes(userId)) {
				return prev.filter((id) => id !== userId);
			}
			return [...prev, userId];
		});
	}, []);
	const createChannel = useCallback(
		async (userIds: Array<string>) => {
			if (restriction) return;
			if (userIds.length !== 1 && blockIfAccountLimited()) return;
			setIsCreating(true);
			try {
				const channel =
					userIds.length === 0
						? await PrivateChannelCommands.createGroupDM([])
						: userIds.length === 1
							? await PrivateChannelCommands.create(userIds[0])
							: await PrivateChannelCommands.createGroupDM(userIds);
				ModalCommands.popWithKey(UNADDABLE_RECIPIENTS_MODAL_KEY);
				ModalCommands.popWithKey(DUPLICATE_GROUP_MODAL_KEY);
				if (autoCloseOnCreate) {
					ModalCommands.pop();
				}
				NavigationCommands.selectChannel(ME, channel.id);
				focusChannelTextareaAfterNavigation(channel.id);
			} catch (error) {
				const unaddable = extractUnaddableRecipientsPayload(error);
				if (unaddable) {
					const addableRecipients = unaddable.addable_recipients;
					const unaddableRecipients: Array<UnaddableRecipient> = unaddable.unaddable_recipients.map((entry) => ({
						userId: entry.user_id,
						reason: entry.reason,
					}));
					const unaddableSet = new Set(unaddableRecipients.map((r) => r.userId));
					setSelectedUserIds((prev) => prev.filter((id) => !unaddableSet.has(id)));
					ModalCommands.popWithKey(DUPLICATE_GROUP_MODAL_KEY);
					ModalCommands.pushWithKey(
						modal(() => (
							<UnaddableRecipientsConfirmModal
								unaddableRecipients={unaddableRecipients}
								addableCount={addableRecipients.length}
								onConfirm={async () => {
									await createChannel(addableRecipients);
								}}
								data-flx="channel.create-dm-modal-utils.create-channel.unaddable-recipients-confirm-modal"
							/>
						)),
						UNADDABLE_RECIPIENTS_MODAL_KEY,
					);
				} else {
					logger.error('Failed to create DM:', error);
					showDmActionErrorModal(error);
				}
			} finally {
				setIsCreating(false);
			}
		},
		[autoCloseOnCreate, restriction],
	);
	const handleCreate = useCallback(async () => {
		if (isCreating || restriction) return null;
		const selectionSnapshot = [...selectedUserIds];
		if (selectionSnapshot.length > 1) {
			const duplicates = getDuplicateGroupDMChannels(selectionSnapshot, duplicateExcludeChannelId);
			if (duplicates.length > 0) {
				return {duplicates, selectionSnapshot};
			}
		}
		await createChannel(selectionSnapshot);
		return null;
	}, [selectedUserIds, isCreating, restriction, createChannel, duplicateExcludeChannelId]);
	const buttonText = useMemo(() => {
		if (selectedUserIds.length === 0) {
			return i18n._(CREATE_GROUP_DM_DESCRIPTOR);
		}
		if (selectedUserIds.length === 1) {
			return i18n._(CREATE_DM_DESCRIPTOR);
		}
		return i18n._(CREATE_GROUP_DM_DESCRIPTOR);
	}, [selectedUserIds.length, i18n.locale]);
	const subtitleText = useMemo(() => {
		return i18n._(CHOOSE_FRIENDS_TO_MESSAGE_DESCRIPTOR);
	}, [i18n.locale]);
	return {
		selectedUserIds,
		isCreating,
		searchQuery,
		maxSelections,
		buttonText,
		subtitleText,
		restriction,
		handleToggle,
		handleCreate,
		setSearchQuery,
		handleCreateChannel: createChannel,
	};
}
