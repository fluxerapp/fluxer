// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import * as ChannelCommands from '@app/features/channel/commands/ChannelCommands';
import {DMCloseFailedModal} from '@app/features/channel/components/alerts/DMCloseFailedModal';
import {requestCopyMessageLink} from '@app/features/channel/components/MessageActionUtils';
import {CLOSE_DM_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import {useLeaveGuild} from '@app/features/guild/hooks/useLeaveGuild';
import GuildBans from '@app/features/guild/state/GuildBans';
import {
	CANCEL_DESCRIPTOR,
	CONTINUE_DESCRIPTOR,
	DM_CLOSED_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import Messages from '@app/features/messaging/state/MessagingMessages';
import * as IARCommands from '@app/features/moderation/commands/IARCommands';
import {showModerationErrorModal} from '@app/features/moderation/components/alerts/ModerationErrorModalUtils';
import {BanMemberModal} from '@app/features/moderation/components/modals/BanMemberModal';
import {IARActionCards} from '@app/features/moderation/components/report_modal/IARActionCards';
import {
	getReportCategoryForReason,
	type IARRuleCategoryId,
	type IARRuleReasonId,
} from '@app/features/moderation/components/report_modal/IARFlowUtils';
import styles from '@app/features/moderation/components/report_modal/IARModal.module.css';
import {resolveIARModalContext} from '@app/features/moderation/components/report_modal/IARModalContext';
import {
	getIARActionCards,
	getIARCategoryTitle,
	getIARChildSafetyRoutingNote,
	getIARModalDescription,
	getIARReasonTitle,
	getIARReportEligibilityCopy,
	getIARRuleCategoryOptions,
	getIARRuleReasonOptions,
	getIARSpecialSafetyNote,
	getIARSuccessCopy,
} from '@app/features/moderation/components/report_modal/IARModalCopy';
import {IARModalPreview} from '@app/features/moderation/components/report_modal/IARModalPreview';
import type {IARContext, IARStep} from '@app/features/moderation/components/report_modal/IARModalTypes';
import {canSubmitReport, showReportRestrictionDialog} from '@app/features/moderation/utils/ReportVerificationGate';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as RelationshipActionUtils from '@app/features/relationship/utils/RelationshipActionUtils';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {RadioGroup} from '@app/features/ui/radio_group/RadioGroup';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import {ME} from '@fluxer/constants/src/AppConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useState} from 'react';

export type {IARContext} from '@app/features/moderation/components/report_modal/IARModalTypes';

const CLOSE_YOUR_CURRENT_DM_WITH_THIS_DOES_NOT_DESCRIPTOR = msg({
	message: 'Close your current DM with {dmDisplayName}. This does not block the user, and you can reopen the DM later.',
	comment:
		'Confirmation copy in the IAR modal for the Close DM quick action. Reassures the user the action is not destructive.',
});
const PICK_AN_OPTION_TO_CONTINUE_DESCRIPTOR = msg({
	message: 'Pick an option to continue.',
	comment: 'Validation hint in the IAR modal when the user tries to advance without picking a radio option.',
});
const PICK_THE_RULE_TO_REVIEW_DESCRIPTOR = msg({
	message: 'Pick the rule to review.',
	comment: 'Validation hint in the IAR modal on the rule selection step.',
});
const COULDN_T_SEND_DESCRIPTOR = msg({
	message: "Couldn't send it. Try again.",
	comment: 'Error toast in the IAR modal when the report submission fails. Keep tone plain and short.',
});
const FINISH_ACCOUNT_SETUP_DESCRIPTOR = msg({
	message: 'Finish account setup',
	comment: 'Button label in the IAR modal that opens the account-setup flow.',
});
const BACK_DESCRIPTOR = msg({
	message: 'Back',
	comment: 'Footer button in the IAR modal that returns to the previous step.',
});
const SEND_REPORT_DESCRIPTOR = msg({
	message: 'Send report',
	comment: 'Footer submit button in the IAR modal. Sends the report to the safety team.',
});
const DONE_DESCRIPTOR = msg({
	message: 'Done',
	comment: 'Footer button in the IAR modal on the final / success screen. Closes the modal.',
});

interface IARModalProps {
	context: IARContext;
}

const logger = new Logger('IARModal');
const STEP_ORDER: ReadonlyArray<IARStep> = ['category', 'reason', 'success'];
export const IARModal: React.FC<IARModalProps> = observer(({context}) => {
	const {i18n} = useLingui();
	const leaveGuild = useLeaveGuild();
	const [step, setStep] = useState<IARStep>('category');
	const [selectedCategory, setSelectedCategory] = useState<IARRuleCategoryId | null>(null);
	const [selectedReason, setSelectedReason] = useState<IARRuleReasonId | null>(null);
	const [submitting, setSubmitting] = useState(false);
	const canSubmitPlatformReport = canSubmitReport();
	const resolvedContext = resolveIARModalContext(i18n, context);
	const ruleCategoryOptions = getIARRuleCategoryOptions(i18n);
	const ruleReasonOptions = getIARRuleReasonOptions(i18n, context.type, selectedCategory);
	const childSafetyRoutingNote = getIARChildSafetyRoutingNote(i18n, context.type, selectedReason);
	const safetyNote = step === 'reason' ? getIARSpecialSafetyNote(i18n, selectedReason) : null;
	const reportEligibilityCopy = getIARReportEligibilityCopy(i18n);
	const modalDescription = getIARModalDescription(i18n);
	const isMessageDeleted =
		context.type === 'message' && Messages.getMessage(context.message.channelId, context.message.id) === undefined;
	const isUserKnownBanned =
		resolvedContext.banGuildId !== null &&
		resolvedContext.reportedUser !== null &&
		GuildBans.isKnownBanned(resolvedContext.banGuildId, resolvedContext.reportedUser.id);
	const reportCategory = useMemo(() => {
		if (selectedReason === null) return null;
		return getReportCategoryForReason(context.type, selectedReason);
	}, [context.type, selectedReason]);
	const closeModal = useCallback(() => {
		ModalCommands.pop();
	}, []);
	const handleOpenReportEligibility = useCallback(() => {
		showReportRestrictionDialog();
	}, []);
	const openConnectionsSettings = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<UserSettingsModal
					initialTab="privacy_safety"
					initialSubtab="connections"
					data-flx="moderation.iar-modal.open-connections-settings.user-settings-modal"
				/>
			)),
		);
	}, []);
	const openCommunicationSettings = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<UserSettingsModal
					initialTab="privacy_safety"
					initialSubtab="communication"
					data-flx="moderation.iar-modal.open-communication-settings.user-settings-modal"
				/>
			)),
		);
	}, []);
	const handleBlockUser = useCallback(() => {
		if (resolvedContext.reportedUser === null) return;
		RelationshipActionUtils.showBlockUserConfirmation(i18n, resolvedContext.reportedUser);
	}, [i18n, resolvedContext.reportedUser]);
	const handleLeaveCommunity = useCallback(() => {
		if (resolvedContext.leaveableGuildId === null) return;
		leaveGuild(resolvedContext.leaveableGuildId);
	}, [leaveGuild, resolvedContext.leaveableGuildId]);
	const handleCloseDM = useCallback(() => {
		const dmChannel = resolvedContext.dmChannel;
		const dmDisplayName = resolvedContext.dmDisplayName;
		if (dmChannel === null) return;
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(CLOSE_DM_DESCRIPTOR)}
					description={i18n._(CLOSE_YOUR_CURRENT_DM_WITH_THIS_DOES_NOT_DESCRIPTOR, {dmDisplayName})}
					primaryText={i18n._(CLOSE_DM_DESCRIPTOR)}
					primaryVariant="danger"
					onPrimary={async () => {
						try {
							await ChannelCommands.remove(dmChannel.id);
							const selectedChannel = SelectedChannel.selectedChannelIds.get(ME);
							if (selectedChannel === dmChannel.id) {
								RouterUtils.transitionTo(Routes.ME);
							}
							ToastCommands.createToast({
								type: 'success',
								children: i18n._(DM_CLOSED_DESCRIPTOR),
							});
						} catch (error) {
							logger.error('Failed to close DM:', error);
							window.setTimeout(() => {
								ModalCommands.push(
									modal(() => (
										<DMCloseFailedModal data-flx="moderation.iar-modal.handle-close-dm.dm-close-failed-modal" />
									)),
								);
							}, 0);
						}
					}}
					data-flx="moderation.iar-modal.handle-close-dm.confirm-modal"
				/>
			)),
		);
	}, [i18n, resolvedContext.dmChannel, resolvedContext.dmDisplayName]);
	const handleCopyMessageLink = useCallback(() => {
		if (context.type !== 'message') return;
		requestCopyMessageLink(context.message, i18n);
	}, [context, i18n]);
	const handleBanUser = useCallback(() => {
		if (resolvedContext.banGuildId === null || resolvedContext.reportedUser === null) return;
		const guildId = resolvedContext.banGuildId;
		const target = resolvedContext.reportedUser;
		ModalCommands.push(
			modal(() => (
				<BanMemberModal
					guildId={guildId}
					targetUser={target}
					data-flx="moderation.iar-modal.handle-ban-user.ban-member-modal"
				/>
			)),
		);
	}, [resolvedContext.banGuildId, resolvedContext.reportedUser]);
	const handleDeleteMessage = useCallback(() => {
		if (context.type !== 'message') return;
		MessageCommands.showDeleteConfirmation(i18n, {
			message: context.message,
			suppressSafetyTeamReportToggle: true,
		});
	}, [context, i18n]);
	const actionHandlers = {
		onBlockUser: handleBlockUser,
		onCloseDM: handleCloseDM,
		onCopyMessageLink: handleCopyMessageLink,
		onLeaveCommunity: handleLeaveCommunity,
		onOpenCommunicationSettings: openCommunicationSettings,
		onOpenConnectionsSettings: openConnectionsSettings,
		onDeleteMessage: handleDeleteMessage,
		onBanUser: handleBanUser,
	};
	const successActionCards = getIARActionCards(i18n, context, resolvedContext, actionHandlers, {
		isMessageDeleted,
		isUserBanned: isUserKnownBanned,
	});
	const handleBack = useCallback(() => {
		switch (step) {
			case 'category':
			case 'success':
				closeModal();
				return;
			case 'reason':
				setSelectedReason(null);
				setStep('category');
				return;
		}
	}, [closeModal, step]);
	const handleContinue = useCallback(() => {
		if (selectedCategory === null) {
			showModerationErrorModal(
				i18n,
				() => i18n._(PICK_AN_OPTION_TO_CONTINUE_DESCRIPTOR),
				'moderation.iar-modal.category-required-error-modal',
			);
			return;
		}
		if (!canSubmitPlatformReport) {
			showReportRestrictionDialog();
			return;
		}
		setSelectedReason(null);
		setStep('reason');
	}, [canSubmitPlatformReport, i18n, selectedCategory]);
	const handleSubmit = useCallback(async () => {
		if (!canSubmitPlatformReport) {
			showReportRestrictionDialog();
			return;
		}
		if (selectedReason === null || reportCategory === null) {
			showModerationErrorModal(
				i18n,
				() => i18n._(PICK_THE_RULE_TO_REVIEW_DESCRIPTOR),
				'moderation.iar-modal.reason-required-error-modal',
			);
			return;
		}
		setSubmitting(true);
		try {
			switch (context.type) {
				case 'message':
					await IARCommands.reportMessage(context.message.channelId, context.message.id, reportCategory);
					break;
				case 'user':
					await IARCommands.reportUser(context.user.id, reportCategory, context.guildId);
					break;
				case 'guild':
					await IARCommands.reportGuild(context.guild.id, reportCategory, context.inviteCode);
					break;
			}
			setStep('success');
		} catch (error) {
			logger.error('Failed to submit report:', error);
			showModerationErrorModal(i18n, () => i18n._(COULDN_T_SEND_DESCRIPTOR), 'moderation.iar-modal.submit-error-modal');
		} finally {
			setSubmitting(false);
		}
	}, [canSubmitPlatformReport, context, i18n, reportCategory, selectedReason]);
	const renderCategoryStep = () => (
		<div className={styles.step} data-flx="moderation.iar-modal.render-category-step.step">
			<h2 className={styles.stepTitle} data-flx="moderation.iar-modal.render-category-step.step-title">
				{getIARCategoryTitle(i18n)}
			</h2>
			<IARModalPreview
				context={context}
				currentChannel={resolvedContext.currentChannel}
				data-flx="moderation.iar-modal.render-category-step.iar-modal-preview"
			/>
			{!canSubmitPlatformReport && (
				<div className={styles.notice} data-flx="moderation.iar-modal.render-category-step.notice">
					<div
						className={styles.noticeTitle}
						data-flx="moderation.report-modal.iar-modal.render-category-step.notice-title"
					>
						{reportEligibilityCopy.title}
					</div>
					<div
						className={styles.noticeText}
						data-flx="moderation.report-modal.iar-modal.render-category-step.notice-text"
					>
						{reportEligibilityCopy.body}
					</div>
					<div
						className={styles.noticeActions}
						data-flx="moderation.report-modal.iar-modal.render-category-step.notice-actions"
					>
						<Button
							variant="secondary"
							small
							fitContent
							onClick={handleOpenReportEligibility}
							data-flx="moderation.iar-modal.render-category-step.button.open-report-eligibility"
						>
							{i18n._(FINISH_ACCOUNT_SETUP_DESCRIPTOR)}
						</Button>
					</div>
				</div>
			)}
			<RadioGroup<IARRuleCategoryId>
				options={ruleCategoryOptions}
				value={selectedCategory}
				onChange={setSelectedCategory}
				aria-label={getIARCategoryTitle(i18n)}
				data-flx="moderation.iar-modal.render-category-step.radio-group.set-selected-category"
			/>
		</div>
	);
	const renderReasonStep = () => (
		<div className={styles.step} data-flx="moderation.iar-modal.render-reason-step.step">
			<h2 className={styles.stepTitle} data-flx="moderation.iar-modal.render-reason-step.step-title">
				{getIARReasonTitle(i18n)}
			</h2>
			<IARModalPreview
				context={context}
				currentChannel={resolvedContext.currentChannel}
				data-flx="moderation.iar-modal.render-reason-step.iar-modal-preview"
			/>
			<RadioGroup<IARRuleReasonId>
				options={ruleReasonOptions}
				value={selectedReason}
				onChange={setSelectedReason}
				aria-label={getIARReasonTitle(i18n)}
				data-flx="moderation.iar-modal.render-reason-step.radio-group.set-selected-reason"
			/>
			{childSafetyRoutingNote !== null && (
				<div className={styles.inlineNote} data-flx="moderation.iar-modal.render-reason-step.child-safety-note">
					{childSafetyRoutingNote}
				</div>
			)}
			{safetyNote !== null && (
				<div className={styles.inlineNote} data-flx="moderation.iar-modal.render-reason-step.safety-note">
					{safetyNote}
				</div>
			)}
		</div>
	);
	const renderSuccessStep = () => {
		const successCopy = getIARSuccessCopy(i18n);
		return (
			<div className={styles.step} data-flx="moderation.iar-modal.render-success-step.step">
				<h2 className={styles.stepTitle} data-flx="moderation.iar-modal.render-success-step.step-title">
					{successCopy.title}
				</h2>
				<p className={styles.stepBody} data-flx="moderation.iar-modal.render-success-step.step-body">
					{successCopy.body}
				</p>
				<IARActionCards
					cards={successActionCards}
					data-flx="moderation.iar-modal.render-success-step.iar-action-cards"
				/>
			</div>
		);
	};
	const renderStep = (): React.ReactNode => {
		switch (step) {
			case 'category':
				return renderCategoryStep();
			case 'reason':
				return renderReasonStep();
			case 'success':
				return renderSuccessStep();
		}
	};
	const getSecondaryFooterLabel = (): string => {
		return step === 'category' ? i18n._(CANCEL_DESCRIPTOR) : i18n._(BACK_DESCRIPTOR);
	};
	const showSecondaryFooterButton = step !== 'success';
	const renderPrimaryFooterButton = () => {
		switch (step) {
			case 'category':
				return (
					<Button
						onClick={handleContinue}
						disabled={selectedCategory === null || submitting}
						data-flx="moderation.iar-modal.render-primary-footer-button.button.continue"
					>
						{i18n._(CONTINUE_DESCRIPTOR)}
					</Button>
				);
			case 'reason':
				return (
					<Button
						onClick={handleSubmit}
						disabled={selectedReason === null || submitting}
						submitting={submitting}
						data-flx="moderation.iar-modal.render-primary-footer-button.button.submit"
					>
						{i18n._(SEND_REPORT_DESCRIPTOR)}
					</Button>
				);
			case 'success':
				return (
					<Button
						onClick={closeModal}
						disabled={submitting}
						data-flx="moderation.iar-modal.render-primary-footer-button.button.close-modal"
					>
						{i18n._(DONE_DESCRIPTOR)}
					</Button>
				);
		}
	};
	return (
		<Modal.Root size="small" centered data-flx="moderation.iar-modal.modal-root">
			<Modal.Header title={resolvedContext.title} data-flx="moderation.iar-modal.modal-header" />
			<Modal.Content data-flx="moderation.iar-modal.modal-content">
				<Modal.ContentLayout data-flx="moderation.iar-modal.modal-content-layout">
					<Modal.ScreenReaderLabel text={modalDescription} data-flx="moderation.iar-modal.modal-screen-reader-label" />
					<SteppedCarousel step={step} steps={STEP_ORDER} data-flx="moderation.iar-modal.stepped-carousel">
						{renderStep()}
					</SteppedCarousel>
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="moderation.iar-modal.modal-footer">
				{showSecondaryFooterButton && (
					<Button
						variant="secondary"
						onClick={handleBack}
						disabled={submitting}
						data-flx="moderation.iar-modal.button.back"
					>
						{getSecondaryFooterLabel()}
					</Button>
				)}
				{renderPrimaryFooterButton()}
			</Modal.Footer>
		</Modal.Root>
	);
});
