// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	BLOCK_DESCRIPTION_DESCRIPTOR,
	BLOCK_NAME_DESCRIPTOR,
	BLOCKED_BUTTON_DESCRIPTOR,
	MORE_YOU_CAN_DO_DESCRIPTOR,
	THANK_YOU_BODY_DESCRIPTOR,
	THANK_YOU_NO_REPORT_BODY_DESCRIPTOR,
	THANK_YOU_NO_REPORT_SHORT_BODY_DESCRIPTOR,
} from '@app/features/moderation/components/report_flow/ReportFlowCopy';
import styles from '@app/features/moderation/components/report_flow/ReportFlowThankYou.module.css';
import {BLOCK_DESCRIPTOR} from '@app/features/moderation/utils/ModerationMessageDescriptors';
import Relationships from '@app/features/relationship/state/Relationships';
import * as RelationshipActionUtils from '@app/features/relationship/utils/RelationshipActionUtils';
import {Button} from '@app/features/ui/button/Button';
import type {User} from '@app/features/user/models/User';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface ReportFlowThankYouProps {
	reportSent: boolean;
	productName: string;
	blockTarget: User | null;
	endedOnStart: boolean;
	guildId?: string | null;
	channelId?: string;
}

export const ReportFlowThankYou: React.FC<ReportFlowThankYouProps> = observer(
	({reportSent, productName, blockTarget, endedOnStart, guildId, channelId}) => {
		const {i18n} = useLingui();
		const isBlocked = blockTarget !== null && Relationships.isBlocked(blockTarget.id);
		const blockTargetName = blockTarget !== null ? NicknameUtils.getNickname(blockTarget, guildId, channelId) : '';
		return (
			<div className={styles.container} data-flx="moderation.report-flow.report-flow-thank-you.container">
				<p className={styles.body} data-flx="moderation.report-flow.report-flow-thank-you.body">
					{reportSent
						? i18n._(THANK_YOU_BODY_DESCRIPTOR, {productName})
						: endedOnStart
							? i18n._(THANK_YOU_NO_REPORT_BODY_DESCRIPTOR)
							: i18n._(THANK_YOU_NO_REPORT_SHORT_BODY_DESCRIPTOR)}
				</p>
				{blockTarget !== null && (
					<section className={styles.section} data-flx="moderation.report-flow.report-flow-thank-you.section">
						<h4 className={styles.heading} data-flx="moderation.report-flow.report-flow-thank-you.heading">
							{i18n._(MORE_YOU_CAN_DO_DESCRIPTOR)}
						</h4>
						<div className={styles.box} data-flx="moderation.report-flow.report-flow-thank-you.box">
							<div className={styles.row} data-flx="moderation.report-flow.report-flow-thank-you.row">
								<div className={styles.rowText} data-flx="moderation.report-flow.report-flow-thank-you.row-text">
									<span className={styles.rowTitle} data-flx="moderation.report-flow.report-flow-thank-you.row-title">
										{i18n._(BLOCK_NAME_DESCRIPTOR, {name: blockTargetName})}
									</span>
									<span
										className={styles.rowDescription}
										data-flx="moderation.report-flow.report-flow-thank-you.row-description"
									>
										{i18n._(BLOCK_DESCRIPTION_DESCRIPTOR)}
									</span>
								</div>
								<Button
									variant={isBlocked ? 'secondary' : 'danger'}
									small
									fitContent
									disabled={isBlocked}
									onClick={() =>
										RelationshipActionUtils.showBlockUserConfirmation(i18n, blockTarget, {userName: blockTargetName})
									}
									data-flx="moderation.report-flow.report-flow-thank-you.button.show-block-user-confirmation"
								>
									{isBlocked ? i18n._(BLOCKED_BUTTON_DESCRIPTOR) : i18n._(BLOCK_DESCRIPTOR)}
								</Button>
							</div>
						</div>
					</section>
				)}
			</div>
		);
	},
);
