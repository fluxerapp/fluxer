// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {SettingsSection} from '@app/features/app/components/dialogs/shared/SettingsSection';
import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as OperatorPassCommands from '@app/features/premium/commands/OperatorPassCommands';
import {OperatorPassWallNameField} from '@app/features/premium/components/operator_pass/OperatorPassWallNameField';
import OperatorPassHoldings from '@app/features/premium/state/OperatorPassHoldings';
import {
	formatOperatorPassDate,
	formatOperatorPassDateTime,
	getOperatorPassWallNameProblem,
	isOperatorPassAvailable,
	normalizeOperatorPassWallName,
	type OperatorPassWallNameProblem,
} from '@app/features/premium/utils/OperatorPassUtils';
import {
	OPERATOR_PASS_WALL_SECTION_DESCRIPTOR,
	WALL_SAVE_FAILED_DESCRIPTOR,
} from '@app/features/premium/utils/PremiumMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import styles from '@app/features/user/components/modals/tabs/operator_pass_tab/OperatorPassWallSettings.module.css';
import Users from '@app/features/user/state/Users';
import {getDefaultAvatarURL, getUserAvatarURL} from '@app/features/user/utils/AvatarUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {OperatorPassListingResponse} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {type ReactNode, useEffect, useRef, useState} from 'react';

const WALL_HOW_IT_WORKS_DESCRIPTOR = msg({
	message: 'How the wall works',
	comment: "Link under the Operators' wall settings. It opens the question about the wall on the Operator Pass page.",
});
const WALL_NOT_LISTED_DESCRIPTOR = msg({
	message: 'Not listed.',
	comment: "Status in the Operators' wall settings when the account chose not to be listed or has not chosen a name.",
});
const WALL_ON_THE_WALL_DESCRIPTOR = msg({
	message: 'On the wall',
	comment: "Label in front of the name and avatar the Operators' wall shows for this account right now.",
});
const WALL_IN_REVIEW_DESCRIPTOR = msg({
	message: 'In review',
	comment: "Label in front of a new name and avatar for the Operators' wall that our team has not checked yet.",
});
const WALL_REVIEW_BY_DESCRIPTOR = msg({
	message: 'by {date}',
	comment:
		"Short muted text after a name waiting for review on the Operators' wall. {date} is the date our team aims to finish the review by, without a time.",
});
const WALL_LIVE_DESCRIPTOR = msg({
	context: 'operators-wall-status',
	message: 'Live',
	comment: "Status next to the name and avatar when they are approved and shown on the Operators' wall.",
});
const WALL_DECLINED_DESCRIPTOR = msg({
	message: 'Declined: {name}. {reason}.',
	comment:
		'Status in the Operators\' wall settings when our team declined a new name or avatar. {name} is the declined name. {reason} is one of the short decline reasons, such as "Breaks the community guidelines".',
});
const WALL_TEAM_NOTE_DESCRIPTOR = msg({
	message: 'Team note: {note}',
	comment:
		"Line in the Operators' wall settings with what our team wrote about a declined name or avatar. {note} is their text, in the language they wrote it.",
});
const WALL_HIDDEN_DESCRIPTOR = msg({
	message: 'Hidden by our team for now.',
	comment: "Status in the Operators' wall settings when our team hid the listing for a while.",
});
const WALL_HIDDEN_WHY_DESCRIPTOR = msg({
	message: 'Why?',
	comment: "Link after the hidden status in the Operators' wall settings. It opens the Operator Pass terms.",
});
const WALL_REMOVED_DESCRIPTOR = msg({
	message: 'Off the wall while your badge and Operator role are removed.',
	comment:
		"Status in the Operators' wall settings while our team has removed the Operator badge, the wall listing and the Operator role from the account under the Operator Pass terms. Operator is the role name, keep it as is. A link to the terms follows it.",
});
const WALL_TERMS_LINK_DESCRIPTOR = msg({
	message: 'Operator Pass terms',
	comment: 'Link to the Operator Pass terms. Operator Pass is a product name.',
});
const WALL_SEND_CHANGES_DESCRIPTOR = msg({
	message: 'Send profile changes to the wall',
	comment:
		"Switch in the Operators' wall settings. When on, new profile names and avatars go to the Operators' wall after our team checks them.",
});
const WALL_RELEASE_LIST_DESCRIPTOR = msg({
	message: 'Add my name to the yearly Operators list',
	comment:
		"Switch in the Operators' wall settings, off by default. When on, the name on the Operators' wall goes into The Operators list for the year, which Fluxer publishes once a year. Operators are the people who have an Operator Pass, keep the word Operators in English.",
});
const WALL_RELEASE_LIST_HINT_DESCRIPTOR = msg({
	message:
		"Each yearly list of Operators that we publish goes into the notes of a Fluxer release and a file in its source code. Your wall name is only on it if you turn this on. If you ask us to remove it later, we take it out of future lists, the release notes and that file, but earlier versions of the source code and copies others made can't be recalled.",
	comment:
		"Description under the switch that adds the Operators' wall name to the yearly Operators list. A Fluxer release is a published version of the app. Operators are the people who have an Operator Pass, keep the word Operators in English.",
});
const REASON_GUIDELINES_DESCRIPTOR = msg({
	message: 'Breaks the community guidelines',
	comment: "Short reason our team declined a name or avatar on the Operators' wall.",
});
const REASON_IMPERSONATION_DESCRIPTOR = msg({
	message: 'Pretends to be someone else',
	comment: "Short reason our team declined a name or avatar on the Operators' wall.",
});
const REASON_RIGHTS_DESCRIPTOR = msg({
	message: 'Uses a name or image without the rights to it',
	comment: "Short reason our team declined a name or avatar on the Operators' wall.",
});
const REASON_AGE_DESCRIPTOR = msg({
	message: 'Not suitable for younger users',
	comment: "Short reason our team declined a name or avatar on the Operators' wall.",
});
const WALL_SAVE_DESCRIPTOR = msg({
	message: 'Save',
	comment: "Button that saves the Operators' wall settings.",
});
const WALL_SAVED_DESCRIPTOR = msg({
	message: 'Saved.',
	comment: "Toast after saving the Operators' wall settings.",
});

const logger = new Logger('OperatorPassWallSettings');

function declineReasonDescriptor(reason: NonNullable<OperatorPassListingResponse['decline_reason']>) {
	switch (reason) {
		case 'guidelines':
			return REASON_GUIDELINES_DESCRIPTOR;
		case 'impersonation':
			return REASON_IMPERSONATION_DESCRIPTOR;
		case 'rights':
			return REASON_RIGHTS_DESCRIPTOR;
		case 'age_suitability':
			return REASON_AGE_DESCRIPTOR;
	}
}

function WallRow({
	label,
	avatarUrl,
	name,
	trailing,
	flx,
}: {
	label: string;
	avatarUrl: string | null;
	name: string;
	trailing?: ReactNode;
	flx: string;
}) {
	return (
		<div className={styles.row} data-flx={`user.operator-pass-tab.operator-pass-wall-settings.${flx}`}>
			{avatarUrl && (
				<img
					src={avatarUrl}
					alt=""
					className={styles.rowAvatar}
					data-flx="user.operator-pass-tab.operator-pass-wall-settings.row-avatar"
				/>
			)}
			<div className={styles.rowText} data-flx="user.operator-pass-tab.operator-pass-wall-settings.row-entry">
				<span
					className={styles.rowName}
					dir="auto"
					data-flx="user.operator-pass-tab.operator-pass-wall-settings.row-name"
				>
					{name}
				</span>
				<span className={styles.rowMeta} data-flx="user.operator-pass-tab.operator-pass-wall-settings.row-label">
					{label}
					{trailing && (
						<>
							<span aria-hidden="true" data-flx="user.operator-pass-tab.operator-pass-wall-settings.row-separator">
								{' · '}
							</span>
							<span data-flx="user.operator-pass-tab.operator-pass-wall-settings.row-trailing">{trailing}</span>
						</>
					)}
				</span>
			</div>
		</div>
	);
}

const OperatorPassWallStatus = observer(function OperatorPassWallStatus({
	listing,
}: {
	listing: OperatorPassListingResponse;
}) {
	const {i18n} = useLingui();
	const user = Users.currentUser;
	if (listing.status !== 'listed' && listing.display_state !== 'removed') {
		return (
			<p className={styles.status} data-flx="user.operator-pass-tab.operator-pass-wall-settings.status">
				{i18n._(WALL_NOT_LISTED_DESCRIPTOR)}
			</p>
		);
	}
	if (listing.display_state === 'hidden') {
		return (
			<p className={styles.status} data-flx="user.operator-pass-tab.operator-pass-wall-settings.status">
				{i18n._(WALL_HIDDEN_DESCRIPTOR)}{' '}
				<ExternalLink
					href={`${Routes.operatorPassTerms()}#3-1-your-listing`}
					data-flx="user.operator-pass-tab.operator-pass-wall-settings.link.why-hidden"
				>
					{i18n._(WALL_HIDDEN_WHY_DESCRIPTOR)}
				</ExternalLink>
			</p>
		);
	}
	if (listing.display_state === 'removed') {
		return (
			<p className={styles.status} data-flx="user.operator-pass-tab.operator-pass-wall-settings.status">
				{i18n._(WALL_REMOVED_DESCRIPTOR)}{' '}
				<ExternalLink
					href={`${Routes.operatorPassTerms()}#8-when-we-remove-the-bonus`}
					data-flx="user.operator-pass-tab.operator-pass-wall-settings.link.terms"
				>
					{i18n._(WALL_TERMS_LINK_DESCRIPTOR)}
				</ExternalLink>
			</p>
		);
	}
	const avatarUrl = listing.shown_avatar_url ?? (user ? getDefaultAvatarURL(user.id) : null);
	const pending = listing.review_state === 'pending' && listing.pending_name;
	const declined = listing.review_state === 'declined';
	const live = listing.display_state === 'live' && !pending && !declined;
	return (
		<div className={styles.rows} data-flx="user.operator-pass-tab.operator-pass-wall-settings.rows">
			<WallRow
				label={i18n._(WALL_ON_THE_WALL_DESCRIPTOR)}
				avatarUrl={avatarUrl}
				name={listing.shown_name ?? ''}
				trailing={live ? i18n._(WALL_LIVE_DESCRIPTOR) : null}
				flx="shown"
				data-flx="user.operator-pass-tab.operator-pass-wall-settings.operator-pass-wall-status.wall-row"
			/>
			{pending && (
				<WallRow
					label={i18n._(WALL_IN_REVIEW_DESCRIPTOR)}
					avatarUrl={user ? getUserAvatarURL({id: user.id, avatar: user.avatar}) : null}
					name={listing.pending_name ?? ''}
					trailing={
						listing.review_due_at ? (
							<Tooltip
								text={formatOperatorPassDateTime(listing.review_due_at)}
								data-flx="user.operator-pass-tab.operator-pass-wall-settings.review-due-tooltip"
							>
								<time
									dateTime={listing.review_due_at}
									data-flx="user.operator-pass-tab.operator-pass-wall-settings.review-due"
								>
									{i18n._(WALL_REVIEW_BY_DESCRIPTOR, {date: formatOperatorPassDate(listing.review_due_at)})}
								</time>
							</Tooltip>
						) : null
					}
					flx="pending"
					data-flx="user.operator-pass-tab.operator-pass-wall-settings.operator-pass-wall-status.wall-row--2"
				/>
			)}
			{declined && listing.declined_name && listing.decline_reason && (
				<p className={styles.declined} data-flx="user.operator-pass-tab.operator-pass-wall-settings.declined">
					{i18n._(WALL_DECLINED_DESCRIPTOR, {
						name: listing.declined_name,
						reason: i18n._(declineReasonDescriptor(listing.decline_reason)),
					})}
				</p>
			)}
			{declined && listing.decline_note && (
				<p className={styles.status} data-flx="user.operator-pass-tab.operator-pass-wall-settings.decline-note">
					{i18n._(WALL_TEAM_NOTE_DESCRIPTOR, {note: listing.decline_note})}
				</p>
			)}
		</div>
	);
});

export const OperatorPassWallSettings = observer(function OperatorPassWallSettings() {
	const {i18n} = useLingui();
	const [listing, setListing] = useState<OperatorPassListingResponse | null>(null);
	const [name, setName] = useState('');
	const [optOut, setOptOut] = useState(false);
	const [sendChanges, setSendChanges] = useState(true);
	const [releaseListOptIn, setReleaseListOptIn] = useState(false);
	const [problem, setProblem] = useState<OperatorPassWallNameProblem | null>(null);
	const [saving, setSaving] = useState(false);
	const appliedRef = useRef<{
		name: string;
		optOut: boolean;
		sendChanges: boolean;
		releaseListOptIn: boolean;
	} | null>(null);
	const editedRef = useRef(false);
	const applied = appliedRef.current;
	editedRef.current =
		applied !== null &&
		(name !== applied.name ||
			optOut !== applied.optOut ||
			sendChanges !== applied.sendChanges ||
			releaseListOptIn !== applied.releaseListOptIn);
	const apply = (result: OperatorPassListingResponse) => {
		const next = {
			name: result.wall_name ?? Users.currentUser?.displayName ?? '',
			optOut: result.wall_opt_out,
			sendChanges: !result.keep_as_is,
			releaseListOptIn: result.release_list_opt_in,
		};
		appliedRef.current = next;
		setListing(result);
		setName(next.name);
		setOptOut(next.optOut);
		setSendChanges(next.sendChanges);
		setReleaseListOptIn(next.releaseListOptIn);
	};
	const passCount = OperatorPassHoldings.passes?.count;
	useEffect(() => {
		if (!isOperatorPassAvailable()) return;
		let active = true;
		OperatorPassCommands.fetchListing()
			.then((result) => {
				if (!active) return;
				if (editedRef.current) {
					setListing(result);
				} else {
					apply(result);
				}
			})
			.catch((error) => logger.warn("Failed to load the Operators' wall listing", error));
		return () => {
			active = false;
		};
	}, [passCount]);
	const removed = listing?.display_state === 'removed';
	if (!listing || (!listing.has_wall && !removed)) return null;
	const nameChanged =
		optOut !== listing.wall_opt_out || (!optOut && normalizeOperatorPassWallName(name) !== (listing.wall_name ?? ''));
	const switchChanged =
		!optOut && (sendChanges === listing.keep_as_is || releaseListOptIn !== listing.release_list_opt_in);
	const handleSave = async () => {
		const nextProblem = getOperatorPassWallNameProblem(name, optOut);
		setProblem(nextProblem);
		if (nextProblem) return;
		setSaving(true);
		try {
			const result = await OperatorPassCommands.updateListing({
				...(nameChanged ? {wall_name: optOut ? null : normalizeOperatorPassWallName(name)} : {}),
				wall_opt_out: optOut,
				...(optOut ? {} : {keep_as_is: !sendChanges, release_list_opt_in: releaseListOptIn}),
			});
			apply(result);
			ToastCommands.success(i18n._(WALL_SAVED_DESCRIPTOR));
		} catch (error) {
			const code = error instanceof HttpError ? (error.body as {code?: string} | null)?.code : undefined;
			if (code === APIErrorCodes.OPERATOR_PASS_WALL_NAME_REQUIRED) {
				setProblem('required');
			} else if (code === APIErrorCodes.OPERATOR_PASS_WALL_NAME_REJECTED) {
				setProblem('rejected');
			} else {
				ToastCommands.error(i18n._(WALL_SAVE_FAILED_DESCRIPTOR));
			}
		} finally {
			setSaving(false);
		}
	};
	return (
		<SettingsSection
			id="operator-pass-wall"
			title={i18n._(OPERATOR_PASS_WALL_SECTION_DESCRIPTOR)}
			data-flx="user.operator-pass-tab.operator-pass-wall-settings.section"
		>
			<OperatorPassWallStatus
				listing={listing}
				data-flx="user.operator-pass-tab.operator-pass-wall-settings.operator-pass-wall-status"
			/>
			{!removed && (
				<>
					<OperatorPassWallNameField
						id="operator-pass-settings-wall-name"
						name={name}
						optOut={optOut}
						problem={problem}
						disabled={saving}
						context="settings"
						onNameChange={(value) => {
							setName(value);
							if (problem) setProblem(getOperatorPassWallNameProblem(value, optOut));
						}}
						onOptOutChange={(value) => {
							setOptOut(value);
							if (value) setProblem(null);
						}}
						data-flx="user.operator-pass-tab.operator-pass-wall-settings.operator-pass-settings-wall-name"
					/>
					{!optOut && (
						<Switch
							label={i18n._(WALL_SEND_CHANGES_DESCRIPTOR)}
							value={sendChanges}
							onChange={setSendChanges}
							disabled={saving}
							data-flx="user.operator-pass-tab.operator-pass-wall-settings.switch.send-changes"
						/>
					)}
					{!optOut && (
						<Switch
							label={i18n._(WALL_RELEASE_LIST_DESCRIPTOR)}
							description={i18n._(WALL_RELEASE_LIST_HINT_DESCRIPTOR)}
							value={releaseListOptIn}
							onChange={setReleaseListOptIn}
							disabled={saving}
							data-flx="user.operator-pass-tab.operator-pass-wall-settings.switch.release-list"
						/>
					)}
					<div className={styles.actions} data-flx="user.operator-pass-tab.operator-pass-wall-settings.actions">
						<Button
							small
							variant="primary"
							onClick={() => void handleSave()}
							submitting={saving}
							disabled={!nameChanged && !switchChanged}
							data-flx="user.operator-pass-tab.operator-pass-wall-settings.button.save"
						>
							{i18n._(WALL_SAVE_DESCRIPTOR)}
						</Button>
					</div>
				</>
			)}
			<ExternalLink
				href={`${Routes.operatorPassInfo()}#wall`}
				className={styles.howLink}
				data-flx="user.operator-pass-tab.operator-pass-wall-settings.link.how-the-wall-works"
			>
				{i18n._(WALL_HOW_IT_WORKS_DESCRIPTOR)}
			</ExternalLink>
		</SettingsSection>
	);
});
