// SPDX-License-Identifier: AGPL-3.0-or-later

import {ATTACH_FILES_PERMISSION, SEND_MESSAGES_PERMISSION} from '@app/features/app/config/I18nDisplayConstants';
import RequiredActionModal from '@app/features/auth/components/modals/RequiredActionModal';
import {
	DeveloperOptionCheckbox,
	DeveloperOptionRadioItems,
	DeveloperOptionRadioSubmenu,
} from '@app/features/channel/components/channel_header_components/developer_tools/DeveloperToolsMenuComponents';
import * as DeveloperOptionsCommands from '@app/features/devtools/commands/DeveloperOptionsCommands';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemSlider} from '@app/features/ui/action_menu/MenuItemSlider';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import Users from '@app/features/user/state/Users';
import {msg, plural} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {PhoneIcon, TrashIcon, UsersIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {
	getCountdownTimerOptions,
	getGiftDurationOptions,
	getMatureContentChannelGateOptions,
	getMatureContentMediaGateOptions,
	getRequiredActionModeOptions,
	getRequiredActionPhoneStepOptions,
	getRequiredActionResendOutcomeOptions,
	getRequiredActionTabOptions,
	getVerificationBarrierOptions,
} from './OptionPresets';
import {updateOption} from './ResetOptions';

const VERIFICATION_MEMBERSHIP_BARRIERS_DESCRIPTOR = msg({
	message: 'Verification & membership barriers',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const COUNTDOWN_TIMER_DESCRIPTOR = msg({
	message: 'Countdown timer',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const FORCE_NO_PERMISSION_DESCRIPTOR = msg({
	message: 'Force no {permissionName} permission',
	comment: 'Developer tools checkbox label. permissionName is a permission constant display name.',
});
const CHANNEL_PERMISSION_OVERRIDES_DESCRIPTOR = msg({
	message: 'Channel permission overrides',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const SLOWMODE_SIMULATION_DESCRIPTOR = msg({
	message: 'Slowmode simulation',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const SLOWMODE_TIME_REMAINING_DESCRIPTOR = msg({
	message: 'Slowmode time remaining',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const REQUIRED_ACTION_FLOW_DESCRIPTOR = msg({
	message: 'Required action flow',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const MOCK_VARIANT_DESCRIPTOR = msg({
	message: 'Mock variant',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const MOCK_INCOMING_CALL_DESCRIPTOR = msg({
	message: 'Mock incoming call',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const DEFAULT_TAB_DESCRIPTOR = msg({
	message: 'Default tab',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const PHONE_STEP_DESCRIPTOR = msg({
	message: 'Phone step',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const RESEND_OUTCOME_DESCRIPTOR = msg({
	message: 'Resend outcome',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const CONTENT_SAFETY_GATES_DESCRIPTOR = msg({
	message: 'Content safety gates',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const MOCK_MATURE_CHANNEL_GATE_REASON_DESCRIPTOR = msg({
	message: 'Mock mature content channel gate reason',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const MOCK_MATURE_MEDIA_GATE_REASON_DESCRIPTOR = msg({
	message: 'Mock mature content media gate reason',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const GIFT_INVENTORY_FLOW_DESCRIPTOR = msg({
	message: 'Gift inventory flow',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const GIFT_DURATION_DESCRIPTOR = msg({
	message: 'Gift duration',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const MOCKED_PRODUCT_FLOWS_DESCRIPTOR = msg({
	message: 'Mocked product flows',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const VerificationMembershipBarriersMenu: React.FC = observer(() => {
	const {i18n} = useLingui();
	const showCountdown =
		DeveloperOptions.mockVerificationBarrier === 'account_too_new' ||
		DeveloperOptions.mockVerificationBarrier === 'not_member_long';
	return (
		<MenuItemSubmenu
			label={i18n._(VERIFICATION_MEMBERSHIP_BARRIERS_DESCRIPTOR)}
			render={() => (
				<>
					<DeveloperOptionRadioItems
						optionKey="mockVerificationBarrier"
						options={getVerificationBarrierOptions()}
						data-flx="channel.channel-header-components.developer-tools-context-menu.verification-membership-barriers-menu.developer-option-radio-items"
					/>
					{showCountdown && (
						<DeveloperOptionRadioSubmenu
							label={i18n._(COUNTDOWN_TIMER_DESCRIPTOR)}
							optionKey="mockBarrierTimeRemaining"
							options={getCountdownTimerOptions(i18n)}
							selectedValue={DeveloperOptions.mockBarrierTimeRemaining ?? 300000}
							data-flx="channel.channel-header-components.developer-tools-context-menu.verification-membership-barriers-menu.developer-option-radio-submenu"
						/>
					)}
				</>
			)}
			data-flx="channel.channel-header-components.developer-tools-context-menu.verification-membership-barriers-menu.menu-item-submenu"
		/>
	);
});
const ChannelPermissionOverridesMenu: React.FC = () => {
	const {i18n} = useLingui();
	return (
		<MenuItemSubmenu
			label={i18n._(CHANNEL_PERMISSION_OVERRIDES_DESCRIPTOR)}
			render={() => (
				<>
					<DeveloperOptionCheckbox
						optionKey="forceNoSendMessages"
						data-flx="channel.channel-header-components.developer-tools-context-menu.channel-permission-overrides-menu.developer-option-checkbox"
					>
						{i18n._(FORCE_NO_PERMISSION_DESCRIPTOR, {permissionName: SEND_MESSAGES_PERMISSION})}
					</DeveloperOptionCheckbox>
					<DeveloperOptionCheckbox
						optionKey="forceNoAttachFiles"
						data-flx="channel.channel-header-components.developer-tools-context-menu.channel-permission-overrides-menu.developer-option-checkbox--2"
					>
						{i18n._(FORCE_NO_PERMISSION_DESCRIPTOR, {permissionName: ATTACH_FILES_PERMISSION})}
					</DeveloperOptionCheckbox>
				</>
			)}
			data-flx="channel.channel-header-components.developer-tools-context-menu.channel-permission-overrides-menu.menu-item-submenu"
		/>
	);
};
const SlowmodeSimulationMenu: React.FC = observer(() => {
	const {i18n} = useLingui();
	return (
		<MenuItemSubmenu
			label={i18n._(SLOWMODE_SIMULATION_DESCRIPTOR)}
			render={() => (
				<>
					<DeveloperOptionCheckbox
						optionKey="mockSlowmodeActive"
						data-flx="channel.channel-header-components.developer-tools-context-menu.slowmode-simulation-menu.developer-option-checkbox"
					>
						<Trans>Force slowmode active</Trans>
					</DeveloperOptionCheckbox>
					{DeveloperOptions.mockSlowmodeActive && (
						<MenuItemSlider
							label={i18n._(SLOWMODE_TIME_REMAINING_DESCRIPTOR)}
							value={DeveloperOptions.mockSlowmodeRemaining}
							minValue={0}
							maxValue={60000}
							onChange={(value) => updateOption('mockSlowmodeRemaining', Math.round(value / 1000) * 1000)}
							onFormat={(value) => plural({count: Math.floor(value / 1000)}, {one: '# second', other: '# seconds'})}
							data-flx="channel.channel-header-components.developer-tools-context-menu.slowmode-simulation-menu.menu-item-slider.update-option"
						/>
					)}
				</>
			)}
			data-flx="channel.channel-header-components.developer-tools-context-menu.slowmode-simulation-menu.menu-item-submenu"
		/>
	);
});
const RequiredActionFlowMenu: React.FC<{onOpenOverlay: () => void}> = observer(({onOpenOverlay}) => {
	const {i18n} = useLingui();
	const shouldShowDefaultTab = DeveloperOptions.mockRequiredActionsMode === 'email_or_phone';
	const shouldShowPhoneStep =
		DeveloperOptions.mockRequiredActionsMode === 'phone' ||
		(DeveloperOptions.mockRequiredActionsMode === 'email_or_phone' &&
			DeveloperOptions.mockRequiredActionsSelectedTab === 'phone');
	return (
		<MenuItemSubmenu
			label={i18n._(REQUIRED_ACTION_FLOW_DESCRIPTOR)}
			render={() => (
				<>
					<DeveloperOptionRadioSubmenu
						label={i18n._(MOCK_VARIANT_DESCRIPTOR)}
						optionKey="mockRequiredActionsMode"
						options={getRequiredActionModeOptions()}
						data-flx="channel.channel-header-components.developer-tools-context-menu.required-action-flow-menu.developer-option-radio-submenu"
					/>
					<MenuItem
						disabled={!Users.currentUser}
						onClick={onOpenOverlay}
						data-flx="channel.channel-header-components.developer-tools-context-menu.required-action-flow-menu.menu-item.open-overlay"
					>
						<Trans>Open overlay</Trans>
					</MenuItem>
					{shouldShowDefaultTab && (
						<DeveloperOptionRadioSubmenu
							label={i18n._(DEFAULT_TAB_DESCRIPTOR)}
							optionKey="mockRequiredActionsSelectedTab"
							options={getRequiredActionTabOptions()}
							data-flx="channel.channel-header-components.developer-tools-context-menu.required-action-flow-menu.developer-option-radio-submenu--2"
						/>
					)}
					{shouldShowPhoneStep && (
						<DeveloperOptionRadioSubmenu
							label={i18n._(PHONE_STEP_DESCRIPTOR)}
							optionKey="mockRequiredActionsPhoneStep"
							options={getRequiredActionPhoneStepOptions()}
							data-flx="channel.channel-header-components.developer-tools-context-menu.required-action-flow-menu.developer-option-radio-submenu--3"
						/>
					)}
					<DeveloperOptionCheckbox
						optionKey="mockRequiredActionsReverify"
						data-flx="channel.channel-header-components.developer-tools-context-menu.required-action-flow-menu.developer-option-checkbox"
					>
						<Trans>Use reverification text</Trans>
					</DeveloperOptionCheckbox>
					<DeveloperOptionCheckbox
						optionKey="mockRequiredActionsResending"
						data-flx="channel.channel-header-components.developer-tools-context-menu.required-action-flow-menu.developer-option-checkbox--2"
					>
						<Trans>Resend button loading</Trans>
					</DeveloperOptionCheckbox>
					<DeveloperOptionRadioSubmenu
						label={i18n._(RESEND_OUTCOME_DESCRIPTOR)}
						optionKey="mockRequiredActionsResendOutcome"
						options={getRequiredActionResendOutcomeOptions()}
						data-flx="channel.channel-header-components.developer-tools-context-menu.required-action-flow-menu.developer-option-radio-submenu--4"
					/>
				</>
			)}
			data-flx="channel.channel-header-components.developer-tools-context-menu.required-action-flow-menu.menu-item-submenu"
		/>
	);
});
const ContentSafetyGatesMenu: React.FC = () => {
	const {i18n} = useLingui();
	return (
		<MenuItemSubmenu
			label={i18n._(CONTENT_SAFETY_GATES_DESCRIPTOR)}
			render={() => (
				<>
					<DeveloperOptionRadioSubmenu
						label={i18n._(MOCK_MATURE_CHANNEL_GATE_REASON_DESCRIPTOR)}
						optionKey="mockMatureContentGateReason"
						options={getMatureContentChannelGateOptions()}
						data-flx="channel.channel-header-components.developer-tools-context-menu.content-safety-gates-menu.developer-option-radio-submenu"
					/>
					<DeveloperOptionRadioSubmenu
						label={i18n._(MOCK_MATURE_MEDIA_GATE_REASON_DESCRIPTOR)}
						optionKey="mockMatureMediaGateReason"
						options={getMatureContentMediaGateOptions()}
						data-flx="channel.channel-header-components.developer-tools-context-menu.content-safety-gates-menu.developer-option-radio-submenu--2"
					/>
					<DeveloperOptionCheckbox
						optionKey="forceMatureMedia"
						data-flx="channel.channel-header-components.developer-tools-context-menu.content-safety-gates-menu.developer-option-checkbox"
					>
						<Trans>Force mature media</Trans>
					</DeveloperOptionCheckbox>
					<DeveloperOptionCheckbox
						optionKey="mockInUK"
						data-flx="channel.channel-header-components.developer-tools-context-menu.content-safety-gates-menu.developer-option-checkbox--2"
					>
						<Trans>Mock UK geo</Trans>
					</DeveloperOptionCheckbox>
					<DeveloperOptionCheckbox
						optionKey="mockGeoBlocked"
						data-flx="channel.channel-header-components.developer-tools-context-menu.content-safety-gates-menu.developer-option-checkbox--3"
					>
						<Trans>Mock geo block overlay</Trans>
					</DeveloperOptionCheckbox>
				</>
			)}
			data-flx="channel.channel-header-components.developer-tools-context-menu.content-safety-gates-menu.menu-item-submenu"
		/>
	);
};
const GiftInventoryFlowMenu: React.FC = observer(() => {
	const {i18n} = useLingui();
	return (
		<MenuItemSubmenu
			label={i18n._(GIFT_INVENTORY_FLOW_DESCRIPTOR)}
			render={() => (
				<>
					<DeveloperOptionCheckbox
						optionKey="mockGiftInventory"
						checked={DeveloperOptions.mockGiftInventory ?? false}
						uncheckedValue={null}
						onAfterChange={(checked) => {
							if (!checked) {
								updateOption('mockGiftDurationMonths', 12);
								updateOption('mockGiftRedeemed', null);
							}
						}}
						data-flx="channel.channel-header-components.developer-tools-context-menu.gift-inventory-flow-menu.developer-option-checkbox"
					>
						<Trans>Mock gift inventory</Trans>
					</DeveloperOptionCheckbox>
					{DeveloperOptions.mockGiftInventory && (
						<>
							<DeveloperOptionRadioSubmenu
								label={i18n._(GIFT_DURATION_DESCRIPTOR)}
								optionKey="mockGiftDurationMonths"
								options={getGiftDurationOptions()}
								selectedValue={DeveloperOptions.mockGiftDurationMonths ?? 12}
								data-flx="channel.channel-header-components.developer-tools-context-menu.gift-inventory-flow-menu.developer-option-radio-submenu"
							/>
							<DeveloperOptionCheckbox
								optionKey="mockGiftRedeemed"
								checked={DeveloperOptions.mockGiftRedeemed ?? false}
								uncheckedValue={null}
								data-flx="channel.channel-header-components.developer-tools-context-menu.gift-inventory-flow-menu.developer-option-checkbox--2"
							>
								<Trans>Mark as redeemed</Trans>
							</DeveloperOptionCheckbox>
						</>
					)}
				</>
			)}
			data-flx="channel.channel-header-components.developer-tools-context-menu.gift-inventory-flow-menu.menu-item-submenu"
		/>
	);
});
export const MockingMenu: React.FC<{onClose: () => void}> = observer(({onClose}) => {
	const {i18n} = useLingui();
	const openRequiredActionOverlay = () => {
		ModalCommands.pushWithKeyAfterBottomSheetClose(
			onClose,
			ModalCommands.modal(() => (
				<RequiredActionModal
					mock={true}
					data-flx="channel.channel-header-components.developer-tools-context-menu.open-required-action-overlay.required-action-modal"
				/>
			)),
			'required-actions-mock',
		);
	};
	return (
		<MenuItemSubmenu
			label={i18n._(MOCKED_PRODUCT_FLOWS_DESCRIPTOR)}
			render={() => (
				<>
					<VerificationMembershipBarriersMenu data-flx="channel.channel-header-components.developer-tools-context-menu.mocking-menu.verification-membership-barriers-menu" />
					<ChannelPermissionOverridesMenu data-flx="channel.channel-header-components.developer-tools-context-menu.mocking-menu.channel-permission-overrides-menu" />
					<MenuItem
						icon={
							<TrashIcon
								size={remFromPx(16)}
								weight="bold"
								data-flx="channel.channel-header-components.developer-tools-context-menu.mocking-menu.trash-icon"
							/>
						}
						onClick={DeveloperOptionsCommands.clearAllAttachmentMocks}
						closeOnSelect={false}
						data-flx="channel.channel-header-components.developer-tools-context-menu.mocking-menu.menu-item.clear-all-attachment-mocks"
					>
						<Trans>Clear attachment mocks</Trans>
					</MenuItem>
					<MenuItemSubmenu
						label={i18n._(MOCK_INCOMING_CALL_DESCRIPTOR)}
						disabled={!Users.currentUser}
						render={() => (
							<>
								<MenuItem
									icon={
										<PhoneIcon
											size={remFromPx(16)}
											weight="fill"
											data-flx="channel.channel-header-components.developer-tools-context-menu.mocking-menu.menu-item.icon.mock-incoming-call.dm"
										/>
									}
									onClick={DeveloperOptionsCommands.triggerMockIncomingCallDM}
									data-flx="channel.channel-header-components.developer-tools-context-menu.mocking-menu.menu-item.mock-incoming-call.dm"
								>
									<Trans>Direct message</Trans>
								</MenuItem>
								<MenuItem
									icon={
										<UsersIcon
											size={remFromPx(16)}
											weight="fill"
											data-flx="channel.channel-header-components.developer-tools-context-menu.mocking-menu.menu-item.icon.mock-incoming-call.group-dm"
										/>
									}
									onClick={DeveloperOptionsCommands.triggerMockIncomingCallGroupDM}
									data-flx="channel.channel-header-components.developer-tools-context-menu.mocking-menu.menu-item.mock-incoming-call.group-dm"
								>
									<Trans>Group DM</Trans>
								</MenuItem>
							</>
						)}
						data-flx="channel.channel-header-components.developer-tools-context-menu.mocking-menu.menu-item-submenu.mock-incoming-call"
					/>
					<SlowmodeSimulationMenu data-flx="channel.channel-header-components.developer-tools-context-menu.mocking-menu.slowmode-simulation-menu" />
					<RequiredActionFlowMenu
						onOpenOverlay={openRequiredActionOverlay}
						data-flx="channel.channel-header-components.developer-tools-context-menu.mocking-menu.required-action-flow-menu"
					/>
					<ContentSafetyGatesMenu data-flx="channel.channel-header-components.developer-tools-context-menu.mocking-menu.content-safety-gates-menu" />
					<GiftInventoryFlowMenu data-flx="channel.channel-header-components.developer-tools-context-menu.mocking-menu.gift-inventory-flow-menu" />
				</>
			)}
			data-flx="channel.channel-header-components.developer-tools-context-menu.mocking-menu.menu-item-submenu"
		/>
	);
});
