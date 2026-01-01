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
import {Trans, useLingui} from '@lingui/react/macro';
import {ClipboardIcon, QuestionMarkIcon, TicketIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as BetaCodeActionCreators from '~/actions/BetaCodeActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import {
	SettingsTabContainer,
	SettingsTabContent,
	SettingsTabHeader,
} from '~/components/modals/shared/SettingsTabLayout';
import {StatusSlate} from '~/components/modals/shared/StatusSlate';
import {Avatar} from '~/components/uikit/Avatar';
import {Button} from '~/components/uikit/Button/Button';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Spinner} from '~/components/uikit/Spinner';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import type {BetaCodeRecord} from '~/records/BetaCodeRecord';
import BetaCodeStore from '~/stores/BetaCodeStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import * as DateUtils from '~/utils/DateUtils';
import styles from './BetaCodesTab.module.css';

const MAX_UNCLAIMED_BETA_CODES = 6;

const BetaCodeListHeader: React.FC = observer(() => {
	return (
		<div className={styles.listHeader}>
			<div className={styles.listHeaderColumn}>
				<Trans>Code</Trans>
			</div>
			<div className={styles.listHeaderColumn}>
				<Trans>Redeemer</Trans>
			</div>
			<div className={styles.listHeaderColumn}>
				<Trans>Created</Trans>
			</div>
		</div>
	);
});

const BetaCodeListItem: React.FC<{
	betaCode: BetaCodeRecord;
	onRevoke: (code: string) => void;
}> = observer(({betaCode, onRevoke}) => {
	const {i18n} = useLingui();
	const {enabled: isMobile} = MobileLayoutStore;
	const [isHovered, setIsHovered] = React.useState(false);

	const handleCopy = (e: React.MouseEvent) => {
		e.stopPropagation();
		TextCopyActionCreators.copy(i18n, betaCode.code);
	};

	const handleRowClick = () => {
		if (isMobile) {
			TextCopyActionCreators.copy(i18n, betaCode.code);
		}
	};

	const handleKeyDown = (e: React.KeyboardEvent) => {
		if (isMobile && (e.key === 'Enter' || e.key === ' ')) {
			e.preventDefault();
			handleRowClick();
		}
	};

	const createdDate = DateUtils.getFormattedShortDate(betaCode.createdAt);
	const createdTooltip = DateUtils.getFormattedDateTimeWithSeconds(betaCode.createdAt);

	return isMobile ? (
		<div
			role="button"
			tabIndex={0}
			onClick={handleRowClick}
			onKeyDown={handleKeyDown}
			onMouseEnter={() => setIsHovered(true)}
			onMouseLeave={() => setIsHovered(false)}
			className={styles.listItemMobile}
		>
			<div className={styles.codeRow}>
				<span className={styles.labelMobile}>
					<Trans>Code:</Trans>
				</span>
				<code className={styles.code}>{betaCode.code}</code>
			</div>

			<div className={styles.redeemerRow}>
				<span className={styles.labelMobile}>
					<Trans>Redeemer:</Trans>
				</span>
				{betaCode.redeemer ? (
					<>
						<Avatar user={betaCode.redeemer} size={32} className={styles.avatarNoShrink} />
						<div className={styles.redeemerInfo}>
							<span className={styles.redeemerName}>{betaCode.redeemer.tag}</span>
							<span className={styles.redeemerDate}>{DateUtils.getFormattedShortDate(betaCode.redeemedAt!)}</span>
						</div>
					</>
				) : (
					<>
						<div className={styles.unclaimedIcon} style={{width: 32, height: 32}}>
							<QuestionMarkIcon className={styles.unclaimedIconSvg} weight="bold" />
						</div>
						<span className={styles.unclaimedText}>
							<Trans>Unclaimed</Trans>
						</span>
					</>
				)}
			</div>

			<div className={styles.createdRow}>
				<span className={styles.createdLabel}>
					<Trans>Created:</Trans>
				</span>
				<Tooltip text={createdTooltip}>
					<span className={styles.createdDate}>{createdDate}</span>
				</Tooltip>
			</div>

			{!betaCode.redeemer && (
				<Tooltip text={i18n._(msg`Revoke`)}>
					<FocusRing offset={-2}>
						<button
							type="button"
							onClick={(e) => {
								e.stopPropagation();
								onRevoke(betaCode.code);
							}}
							className={styles.revokeButton}
						>
							<XIcon className={styles.revokeButtonIcon} weight="bold" />
						</button>
					</FocusRing>
				</Tooltip>
			)}
		</div>
	) : (
		// biome-ignore lint/a11y/noStaticElementInteractions: Hover state is for visual feedback only, not for interaction
		<div
			onMouseEnter={() => setIsHovered(true)}
			onMouseLeave={() => setIsHovered(false)}
			className={styles.listItemDesktop}
		>
			<div className={styles.codeRow}>
				<span className={styles.labelMobile}>
					<Trans>Code:</Trans>
				</span>
				<code className={styles.code}>{betaCode.code}</code>

				<Tooltip text={i18n._(msg`Click to copy`)}>
					<FocusRing offset={-2}>
						<button
							type="button"
							onClick={handleCopy}
							className={clsx(styles.copyButton, isHovered ? styles.copyButtonVisible : styles.copyButtonHidden)}
							aria-label={i18n._(msg`Copy beta code`)}
						>
							<ClipboardIcon className={styles.copyButtonIcon} />
						</button>
					</FocusRing>
				</Tooltip>
			</div>

			<div className={styles.redeemerRow}>
				<span className={styles.labelMobile}>
					<Trans>Redeemer:</Trans>
				</span>
				{betaCode.redeemer ? (
					<>
						<Avatar user={betaCode.redeemer} size={32} className={styles.avatarNoShrink} />
						<div className={styles.redeemerInfo}>
							<span className={styles.redeemerName}>{betaCode.redeemer.tag}</span>
							<span className={styles.redeemerDate}>{DateUtils.getFormattedShortDate(betaCode.redeemedAt!)}</span>
						</div>
					</>
				) : (
					<>
						<div className={styles.unclaimedIcon} style={{width: 32, height: 32}}>
							<QuestionMarkIcon className={styles.unclaimedIconSvg} weight="bold" />
						</div>
						<span className={styles.unclaimedText}>
							<Trans>Unclaimed</Trans>
						</span>
					</>
				)}
			</div>

			<div className={styles.createdRow}>
				<span className={styles.createdLabel}>
					<Trans>Created:</Trans>
				</span>
				<Tooltip text={createdTooltip}>
					<span className={styles.createdDate}>{createdDate}</span>
				</Tooltip>
			</div>

			{!betaCode.redeemer && (
				<Tooltip text={i18n._(msg`Revoke`)}>
					<FocusRing offset={-2}>
						<button
							type="button"
							onClick={(e) => {
								e.stopPropagation();
								onRevoke(betaCode.code);
							}}
							className={clsx(
								styles.revokeButtonDesktop,
								isHovered ? styles.revokeButtonDesktopVisible : styles.revokeButtonDesktopHidden,
							)}
						>
							<XIcon className={styles.revokeButtonIcon} weight="bold" />
						</button>
					</FocusRing>
				</Tooltip>
			)}
		</div>
	);
});

BetaCodeListItem.displayName = 'BetaCodeListItem';

const BetaCodesTab: React.FC = observer(() => {
	const {i18n} = useLingui();
	const betaCodes = BetaCodeStore.betaCodes;
	const fetchStatus = BetaCodeStore.fetchStatus;
	const allowance = BetaCodeStore.allowance;
	const nextResetAt = BetaCodeStore.nextResetAt;

	React.useEffect(() => {
		BetaCodeActionCreators.fetch();
	}, []);

	const sortedBetaCodes = React.useMemo(() => {
		return [...betaCodes].sort((a, b) => b.createdAt.getTime() - a.createdAt.getTime());
	}, [betaCodes]);

	const unclaimedBetaCodes = sortedBetaCodes.filter((code) => !code.redeemer);
	const redeemedBetaCodes = sortedBetaCodes.filter((code) => code.redeemer);

	const canGenerateMore = unclaimedBetaCodes.length < MAX_UNCLAIMED_BETA_CODES && allowance > 0;

	const handleRevoke = React.useCallback((code: string) => {
		BetaCodeActionCreators.remove(code);
	}, []);

	const handleCreate = React.useCallback(() => {
		BetaCodeActionCreators.create();
	}, []);

	const allowanceText = React.useMemo(() => {
		if (allowance === 0 && nextResetAt !== null) {
			const resetString = DateUtils.getFormattedShortDate(nextResetAt);
			return i18n._(msg`No codes remaining. Resets ${resetString}`);
		}
		if (allowance === 1) {
			return i18n._(msg`1 code remaining this week`);
		}
		return i18n._(msg`${allowance} codes remaining this week`);
	}, [allowance, nextResetAt, i18n]);

	if (fetchStatus === 'pending' || fetchStatus === 'idle') {
		return (
			<div className={styles.spinnerContainer}>
				<Spinner />
			</div>
		);
	}

	if (fetchStatus === 'error') {
		return (
			<StatusSlate
				Icon={TicketIcon}
				title={i18n._(msg`Network error`)}
				description={i18n._(
					msg`We're having trouble connecting to the space-time continuum. Please check your connection and try again.`,
				)}
				actions={[
					{
						text: i18n._(msg`Retry`),
						onClick: () => BetaCodeActionCreators.fetch(),
						variant: 'primary',
					},
				]}
			/>
		);
	}

	return (
		<SettingsTabContainer>
			<SettingsTabHeader
				title={<Trans>Beta Codes</Trans>}
				description={
					<Trans>
						Generate up to {MAX_UNCLAIMED_BETA_CODES} unclaimed beta codes to invite friends to Fluxer. You can create 3
						codes per week.
					</Trans>
				}
			/>

			<SettingsTabContent>
				<div className={styles.actionRow}>
					<div className={styles.allowanceText}>{allowanceText}</div>
					<Button small={true} disabled={!canGenerateMore} onClick={handleCreate}>
						<Trans>Create Code</Trans>
					</Button>
				</div>

				{unclaimedBetaCodes.length > 0 && (
					<div className={styles.subsection}>
						<div className={styles.subsectionTitle}>
							<Trans>Unclaimed ({unclaimedBetaCodes.length})</Trans>
						</div>
						<div className={styles.listContainer}>
							<BetaCodeListHeader />
							<div className={styles.listItems}>
								{unclaimedBetaCodes.map((betaCode) => (
									<BetaCodeListItem key={betaCode.code} betaCode={betaCode} onRevoke={handleRevoke} />
								))}
							</div>
						</div>
					</div>
				)}

				{redeemedBetaCodes.length > 0 && (
					<div className={styles.subsection}>
						<div className={styles.subsectionTitle}>
							<Trans>Redeemed ({redeemedBetaCodes.length})</Trans>
						</div>
						<div className={styles.listContainer}>
							<BetaCodeListHeader />
							<div className={styles.listItems}>
								{redeemedBetaCodes.map((betaCode) => (
									<BetaCodeListItem key={betaCode.code} betaCode={betaCode} onRevoke={handleRevoke} />
								))}
							</div>
						</div>
					</div>
				)}

				{betaCodes.length === 0 && (
					<StatusSlate
						Icon={TicketIcon}
						title={i18n._(msg`No beta codes yet`)}
						description={i18n._(msg`Click "Create Code" to generate your first code.`)}
						actions={[
							{
								text: i18n._(msg`Create Code`),
								onClick: handleCreate,
								variant: canGenerateMore ? 'primary' : 'secondary',
								fitContent: true,
							},
						]}
					/>
				)}
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});

export default BetaCodesTab;
