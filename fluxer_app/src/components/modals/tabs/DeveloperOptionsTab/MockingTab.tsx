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

import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import * as DeveloperOptionsActionCreators from '~/actions/DeveloperOptionsActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {Select} from '~/components/form/Select';
import {Switch} from '~/components/form/Switch';
import RequiredActionModal from '~/components/modals/RequiredActionModal';
import {SettingsTabSection} from '~/components/modals/shared/SettingsTabLayout';
import {Button} from '~/components/uikit/Button/Button';
import {Slider} from '~/components/uikit/Slider';
import type {DeveloperOptionsState} from '~/stores/DeveloperOptionsStore';
import DeveloperOptionsStore from '~/stores/DeveloperOptionsStore';
import UserStore from '~/stores/UserStore';
import styles from './MockingTab.module.css';

export const MockingTabContent: React.FC = observer(() => {
	const {t} = useLingui();
	const handleClearAttachmentMocks = () => {
		DeveloperOptionsActionCreators.clearAllAttachmentMocks();
	};

	return (
		<>
			<SettingsTabSection title={<Trans>Verification Barriers</Trans>}>
				<Select
					label={t`Mock Verification Barrier`}
					value={DeveloperOptionsStore.mockVerificationBarrier}
					options={[
						{value: 'none', label: t`None (Normal Behavior)`},
						{value: 'unclaimed_account', label: t`Unclaimed Account`},
						{value: 'unverified_email', label: t`Unverified Email`},
						{value: 'account_too_new', label: t`Account Too New`},
						{value: 'not_member_long', label: t`Not Member Long Enough`},
						{value: 'no_phone', label: t`No Phone Number`},
						{value: 'send_message_disabled', label: t`Send Message Disabled`},
					]}
					onChange={(value) => {
						DeveloperOptionsActionCreators.updateOption(
							'mockVerificationBarrier',
							value as DeveloperOptionsState['mockVerificationBarrier'],
						);
					}}
				/>
				{(DeveloperOptionsStore.mockVerificationBarrier === 'account_too_new' ||
					DeveloperOptionsStore.mockVerificationBarrier === 'not_member_long') && (
					<Select<string>
						label={t`Countdown Timer`}
						value={DeveloperOptionsStore.mockBarrierTimeRemaining?.toString() ?? '300000'}
						options={[
							{value: '0', label: t`No Timer`},
							{value: '10000', label: t`10 seconds`},
							{value: '30000', label: t`30 seconds`},
							{value: '60000', label: t`1 minute`},
							{value: '120000', label: t`2 minutes`},
							{value: '300000', label: t`5 minutes`},
							{value: '600000', label: t`10 minutes`},
						]}
						onChange={(v) =>
							DeveloperOptionsActionCreators.updateOption('mockBarrierTimeRemaining', Number.parseInt(v, 10))
						}
					/>
				)}
			</SettingsTabSection>

			<SettingsTabSection title={<Trans>Channel Permissions</Trans>}>
				<Switch
					label={t`Force No SEND_MESSAGES Permission`}
					value={DeveloperOptionsStore.forceNoSendMessages}
					description={t`Removes SEND_MESSAGES permission in all channels (disables textarea and all buttons)`}
					onChange={(value) => DeveloperOptionsActionCreators.updateOption('forceNoSendMessages', value)}
				/>
				<Switch
					label={t`Force No ATTACH_FILES Permission`}
					value={DeveloperOptionsStore.forceNoAttachFiles}
					description={t`Removes ATTACH_FILES permission in all channels (hides upload button)`}
					onChange={(value) => DeveloperOptionsActionCreators.updateOption('forceNoAttachFiles', value)}
				/>
			</SettingsTabSection>

			<SettingsTabSection title={<Trans>Attachments</Trans>}>
				<Button variant="secondary" onClick={handleClearAttachmentMocks}>
					<Trans>Clear attachment mocks</Trans>
				</Button>
			</SettingsTabSection>

			<SettingsTabSection title={<Trans>Voice Calls</Trans>}>
				<Button
					variant="secondary"
					onClick={() => DeveloperOptionsActionCreators.triggerMockIncomingCall()}
					disabled={!UserStore.currentUser}
				>
					<Trans>Trigger Mock Incoming Call</Trans>
				</Button>
			</SettingsTabSection>

			<SettingsTabSection title={<Trans>Slowmode</Trans>}>
				<Switch
					label={t`Force Slowmode Active`}
					value={DeveloperOptionsStore.mockSlowmodeActive}
					description={t`Forces slowmode to be active in all channels`}
					onChange={(value) => DeveloperOptionsActionCreators.updateOption('mockSlowmodeActive', value)}
				/>
				{DeveloperOptionsStore.mockSlowmodeActive && (
					<div className={styles.sliderContainer}>
						<div className={styles.sliderLabel}>
							<span className={styles.labelText}>
								<Trans>Slowmode Time Remaining (ms)</Trans>
							</span>
							<p className={styles.labelDescription}>
								<Trans>Set how much time remains before the user can send another message.</Trans>
							</p>
						</div>
						<Slider
							defaultValue={DeveloperOptionsStore.mockSlowmodeRemaining}
							factoryDefaultValue={10000}
							minValue={0}
							maxValue={60000}
							step={1000}
							markers={[0, 5000, 10000, 15000, 20000, 30000, 45000, 60000]}
							stickToMarkers={false}
							onMarkerRender={(v) => `${v / 1000}s`}
							onValueRender={(v) => <Trans>{Math.floor(v / 1000)} seconds</Trans>}
							onValueChange={(v) => DeveloperOptionsActionCreators.updateOption('mockSlowmodeRemaining', v)}
						/>
					</div>
				)}
			</SettingsTabSection>

			<SettingsTabSection
				title={
					<div className={styles.header}>
						<h3 className={styles.headerTitle}>
							<Trans>Visionary</Trans>
						</h3>
						<Button
							variant="secondary"
							small={true}
							onClick={() => {
								DeveloperOptionsActionCreators.updateOption('mockVisionarySoldOut', false);
								DeveloperOptionsActionCreators.updateOption('mockVisionaryRemaining', null);
							}}
						>
							<Trans>Reset to Real Data</Trans>
						</Button>
					</div>
				}
			>
				<Switch
					label={t`Mock Visionary Sold Out`}
					value={DeveloperOptionsStore.mockVisionarySoldOut}
					description={t`Force Visionary to appear sold out`}
					onChange={(value) => DeveloperOptionsActionCreators.updateOption('mockVisionarySoldOut', value)}
				/>
				{!DeveloperOptionsStore.mockVisionarySoldOut && (
					<div className={styles.sliderContainer}>
						<div className={styles.sliderLabel}>
							<span className={styles.labelText}>
								<Trans>Mock Visionary Remaining</Trans>
							</span>
							<p className={styles.labelDescription}>
								<Trans>Set a fixed remaining count for Visionary availability.</Trans>
							</p>
						</div>
						<Slider
							defaultValue={DeveloperOptionsStore.mockVisionaryRemaining ?? 100}
							factoryDefaultValue={100}
							minValue={0}
							maxValue={1000}
							step={10}
							markers={[0, 50, 100, 250, 500, 750, 1000]}
							stickToMarkers={false}
							onMarkerRender={(v) => `${v}`}
							onValueRender={(v) => <Trans>{v} remaining</Trans>}
							onValueChange={(v) => DeveloperOptionsActionCreators.updateOption('mockVisionaryRemaining', v)}
						/>
						<div className={styles.note}>
							<Trans>Set to 0 to simulate sold out.</Trans>
						</div>
					</div>
				)}
			</SettingsTabSection>

			<SettingsTabSection title={<Trans>Required Actions</Trans>}>
				<Select
					label={t`Mock Variant`}
					value={DeveloperOptionsStore.mockRequiredActionsMode}
					description={t`Choose which variant to preview`}
					options={[
						{value: 'email', label: t`Email`},
						{value: 'phone', label: t`Phone`},
						{value: 'email_or_phone', label: t`Email or Phone`},
					]}
					onChange={(value) =>
						DeveloperOptionsActionCreators.updateOption(
							'mockRequiredActionsMode',
							value as DeveloperOptionsState['mockRequiredActionsMode'],
						)
					}
				/>
				<div className={styles.buttonRow}>
					<Button
						onClick={() =>
							ModalActionCreators.pushWithKey(
								modal(() => <RequiredActionModal mock={true} />),
								'required-actions-mock',
							)
						}
						disabled={!UserStore.currentUser}
					>
						<Trans>Open Overlay</Trans>
					</Button>
				</div>

				{DeveloperOptionsStore.mockRequiredActionsMode === 'email_or_phone' && (
					<Select
						label={t`Default Tab`}
						value={DeveloperOptionsStore.mockRequiredActionsSelectedTab}
						description={t`Which tab is selected when the overlay opens`}
						options={[
							{value: 'email', label: t`Email`},
							{value: 'phone', label: t`Phone`},
						]}
						onChange={(value) =>
							DeveloperOptionsActionCreators.updateOption(
								'mockRequiredActionsSelectedTab',
								value as DeveloperOptionsState['mockRequiredActionsSelectedTab'],
							)
						}
					/>
				)}

				{(DeveloperOptionsStore.mockRequiredActionsMode === 'phone' ||
					(DeveloperOptionsStore.mockRequiredActionsMode === 'email_or_phone' &&
						DeveloperOptionsStore.mockRequiredActionsSelectedTab === 'phone')) && (
					<Select
						label={t`Phone Step`}
						value={DeveloperOptionsStore.mockRequiredActionsPhoneStep}
						description={t`Pick which phone step to show`}
						options={[
							{value: 'phone', label: t`Enter Phone`},
							{value: 'code', label: t`Enter Code`},
						]}
						onChange={(value) =>
							DeveloperOptionsActionCreators.updateOption(
								'mockRequiredActionsPhoneStep',
								value as DeveloperOptionsState['mockRequiredActionsPhoneStep'],
							)
						}
					/>
				)}

				<Switch
					label={t`Use Reverification Text`}
					value={DeveloperOptionsStore.mockRequiredActionsReverify}
					description={t`Swap text to reverify variants`}
					onChange={(value) => DeveloperOptionsActionCreators.updateOption('mockRequiredActionsReverify', value)}
				/>

				<Switch
					label={t`Resend Button Loading`}
					value={DeveloperOptionsStore.mockRequiredActionsResending}
					description={t`Force the email resend button into a loading state`}
					onChange={(value) => DeveloperOptionsActionCreators.updateOption('mockRequiredActionsResending', value)}
				/>

				<Select
					label={t`Resend Outcome`}
					value={DeveloperOptionsStore.mockRequiredActionsResendOutcome}
					description={t`Toast shown when clicking resend in mock mode`}
					options={[
						{value: 'success', label: t`Success`},
						{value: 'rate_limited', label: t`Rate Limited`},
						{value: 'server_error', label: t`Server Error`},
					]}
					onChange={(value) =>
						DeveloperOptionsActionCreators.updateOption(
							'mockRequiredActionsResendOutcome',
							value as DeveloperOptionsState['mockRequiredActionsResendOutcome'],
						)
					}
				/>
			</SettingsTabSection>

			<SettingsTabSection title={<Trans>NSFW Gates</Trans>}>
				<Select
					label={t`Mock NSFW Channel Gate Reason`}
					value={DeveloperOptionsStore.mockNSFWGateReason}
					description={t`Mock gate reason for blocking entire NSFW channels`}
					options={[
						{value: 'none', label: t`None (Normal Behavior)`},
						{value: 'geo_restricted', label: t`Geo Restricted`},
						{value: 'age_restricted', label: t`Age Restricted`},
						{value: 'consent_required', label: t`Consent Required`},
					]}
					onChange={(value) => {
						DeveloperOptionsActionCreators.updateOption(
							'mockNSFWGateReason',
							value as DeveloperOptionsState['mockNSFWGateReason'],
						);
					}}
				/>
				<Select
					label={t`Mock NSFW Media Gate Reason`}
					value={DeveloperOptionsStore.mockNSFWMediaGateReason}
					description={t`Forces all media to NSFW and shows the selected blur overlay error state`}
					options={[
						{value: 'none', label: t`None (Normal Behavior)`},
						{value: 'geo_restricted', label: t`Geo Restricted`},
						{value: 'age_restricted', label: t`Age Restricted`},
					]}
					onChange={(value) => {
						DeveloperOptionsActionCreators.updateOption(
							'mockNSFWMediaGateReason',
							value as DeveloperOptionsState['mockNSFWMediaGateReason'],
						);
					}}
				/>
				<Switch
					label={t`Mock Geo Block Overlay`}
					value={DeveloperOptionsStore.mockGeoBlocked}
					description={t`Show the geo-blocked overlay (dismissible)`}
					onChange={(value) => DeveloperOptionsActionCreators.updateOption('mockGeoBlocked', value)}
				/>
			</SettingsTabSection>

			<SettingsTabSection title={<Trans>Gift Inventory</Trans>}>
				<Switch
					label={t`Mock Gift Inventory`}
					value={DeveloperOptionsStore.mockGiftInventory ?? false}
					description={t`Show a mock gift code in your gift inventory for testing`}
					onChange={(value) => {
						DeveloperOptionsActionCreators.updateOption('mockGiftInventory', value ? true : null);
						if (!value) {
							DeveloperOptionsActionCreators.updateOption('mockGiftDurationMonths', 12);
							DeveloperOptionsActionCreators.updateOption('mockGiftRedeemed', null);
						}
					}}
				/>
				{DeveloperOptionsStore.mockGiftInventory && (
					<>
						<Select<string>
							label={t`Gift Duration`}
							value={DeveloperOptionsStore.mockGiftDurationMonths?.toString() ?? '12'}
							description={t`How many months of Plutonium the mock gift provides`}
							options={[
								{value: '1', label: t`1 Month`},
								{value: '3', label: t`3 Months`},
								{value: '6', label: t`6 Months`},
								{value: '12', label: t`12 Months (1 Year)`},
								{value: '0', label: t`Lifetime`},
							]}
							onChange={(value) =>
								DeveloperOptionsActionCreators.updateOption('mockGiftDurationMonths', Number.parseInt(value, 10))
							}
						/>
						<Switch
							label={t`Mark as Redeemed`}
							value={DeveloperOptionsStore.mockGiftRedeemed ?? false}
							description={t`Show the gift as already redeemed`}
							onChange={(value) => DeveloperOptionsActionCreators.updateOption('mockGiftRedeemed', value ? true : null)}
						/>
					</>
				)}
			</SettingsTabSection>
		</>
	);
});
