// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeCrash from '@app/features/app/state/RuntimeCrash';
import {CaptchaModal} from '@app/features/auth/components/modals/CaptchaModal';
import {openClaimAccountModal} from '@app/features/auth/components/modals/ClaimAccountModal';
import {useSudo} from '@app/features/auth/hooks/useSudo';
import NewDeviceMonitoring from '@app/features/auth/state/NewDeviceMonitoring';
import SudoPrompt from '@app/features/auth/state/SudoPrompt';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import {showChannelErrorModalAfterCurrentModal} from '@app/features/channel/components/alerts/ChannelErrorModalUtils';
import {
	openDangerConfirmation,
	PendingMenuItem,
} from '@app/features/channel/components/channel_header_components/developer_tools/DeveloperToolsMenuComponents';
import {resetPremiumStateOverrides} from '@app/features/devtools/components/PremiumScenarioOptions';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import {TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {KeyboardModeIntroModal} from '@app/features/input/components/modals/KeyboardModeIntroModal';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import * as TrustedDomainCommands from '@app/features/trusted_domain/commands/TrustedDomainCommands';
import TrustedDomain from '@app/features/trusted_domain/state/TrustedDomain';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import * as UserCommands from '@app/features/user/commands/UserCommands';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {FLUXERBOT_ID} from '@fluxer/constants/src/AppConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {BugIcon, KeyboardIcon, PhoneIcon, PlugIcon, RobotIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useState} from 'react';

const logger = new Logger('DeveloperToolsContextMenu/ToolsMenu');
const AUTHORIZED_IPS_CLEARED_NEW_LOGINS_WILL_REQUIRE_EMAIL_DESCRIPTOR = msg({
	message: 'Authorized IPs cleared. New logins will require email verification.',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const FAILED_TO_FORGET_AUTHORIZED_IPS_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: 'Failed to forget authorized IPs. Try again.',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const FORGET_AUTHORIZED_IPS_DESCRIPTOR = msg({
	message: 'Forget authorized IPs',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const THIS_REMOVES_ALL_PREVIOUSLY_AUTHORIZED_IP_ADDRESSES_ON_DESCRIPTOR = msg({
	message:
		'This removes all previously authorized IP addresses on your account. The next login from any IP will require email verification.',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const TRUSTED_DOMAINS_CLEARED_DESCRIPTOR = msg({
	message: 'Trusted domains cleared.',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const FAILED_TO_CLEAR_TRUSTED_DOMAINS_DESCRIPTOR = msg({
	message: 'Failed to clear trusted domains.',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const CLEAR_TRUSTED_DOMAINS_DESCRIPTOR = msg({
	message: 'Clear trusted domains',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const THIS_REMOVES_ALL_TRUSTED_DOMAINS_YOU_WILL_SEE_DESCRIPTOR = msg({
	message: 'This removes all trusted domains. You will see the external link warning for all domains again.',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const PREMIUM_STATE_RESET_ON_THE_BACKEND_DESCRIPTOR = msg({
	message: 'Premium state reset on the backend.',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const FAILED_TO_RESET_PREMIUM_STATE_DESCRIPTOR = msg({
	message: 'Failed to reset premium state.',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const RESET_PREMIUM_STATE_DESCRIPTOR = msg({
	message: 'Reset premium state',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const THIS_CLEARS_YOUR_BACKEND_PREMIUM_STATE_ENTIRELY_INCLUDING_DESCRIPTOR = msg({
	message:
		'This clears your backend premium state entirely, including premium type, billing metadata, and override flags.',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const CLEAR_TRUST_ALL_SETTING_DESCRIPTOR = msg({
	message: 'Clear trust all setting',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const NO_TRUSTED_DOMAINS_DESCRIPTOR = msg({
	message: 'No trusted domains',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const CLEAR_TRUSTED_DOMAINS_COUNT_DESCRIPTOR = msg({
	message: 'Clear trusted domains ({trustedDomainsCount})',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const RUNTIME_UTILITIES_DESCRIPTOR = msg({
	message: 'Runtime utilities',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const FORGETTING_AUTHORIZED_IPS_DESCRIPTOR = msg({
	message: 'Forgetting authorized IPs...',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const CLEARING_TRUSTED_DOMAINS_DESCRIPTOR = msg({
	message: 'Clearing trusted domains...',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
const RESETTING_PREMIUM_STATE_DESCRIPTOR = msg({
	message: 'Resetting premium state...',
	comment: 'Developer tools debug menu label. Internal-only surface for developers; translators may keep this terse.',
});
export const ToolsMenu: React.FC<{onClose: () => void}> = observer(({onClose}) => {
	const {i18n} = useLingui();
	const sudo = useSudo();
	const [isForgettingAuthorizedIps, setIsForgettingAuthorizedIps] = useState(false);
	const [isClearingTrustedDomains, setIsClearingTrustedDomains] = useState(false);
	const [isResettingPremiumState, setIsResettingPremiumState] = useState(false);
	const socket = GatewayConnection.socket;
	const trustedDomainsCount = TrustedDomain.getTrustedDomainsCount();
	const handleOpenCaptchaModal = useCallback(() => {
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			ModalCommands.modal(() => (
				<CaptchaModal
					closeOnVerify={false}
					onVerify={(token, captchaType) => {
						logger.debug('Captcha solved from developer tools menu', {token, captchaType});
					}}
					onCancel={() => {
						logger.debug('Captcha cancelled from developer tools menu');
					}}
					data-flx="channel.channel-header-components.developer-tools-context-menu.handle-open-captcha-modal.captcha-modal"
				/>
			)),
		);
	}, [onClose]);
	const handleForgetAuthorizedIps = useCallback(async () => {
		setIsForgettingAuthorizedIps(true);
		try {
			const sudoPayload = await sudo.require();
			await UserCommands.forgetAuthorizedIps(sudoPayload);
			sudo.finalize();
			ToastCommands.createToast({
				type: 'success',
				children: i18n._(AUTHORIZED_IPS_CLEARED_NEW_LOGINS_WILL_REQUIRE_EMAIL_DESCRIPTOR),
			});
		} catch (error) {
			logger.error('Failed to forget authorized IPs', error);
			showChannelErrorModalAfterCurrentModal({
				title: i18n._(FAILED_TO_FORGET_AUTHORIZED_IPS_PLEASE_TRY_AGAIN_DESCRIPTOR),
				message: i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
				dataFlx:
					'channel.channel-header-components.developer-tools-context-menu.forget-authorized-ips-failed.generic-error-modal',
			});
		} finally {
			setIsForgettingAuthorizedIps(false);
		}
	}, [sudo, i18n]);
	const handleForgetAuthorizedIpsClick = useCallback(() => {
		openDangerConfirmation({
			onClose,
			title: i18n._(FORGET_AUTHORIZED_IPS_DESCRIPTOR),
			description: i18n._(THIS_REMOVES_ALL_PREVIOUSLY_AUTHORIZED_IP_ADDRESSES_ON_DESCRIPTOR),
			primaryText: i18n._(FORGET_AUTHORIZED_IPS_DESCRIPTOR),
			onPrimary: handleForgetAuthorizedIps,
		});
	}, [handleForgetAuthorizedIps, onClose, i18n]);
	const handleClearTrustedDomains = useCallback(async () => {
		setIsClearingTrustedDomains(true);
		try {
			await TrustedDomainCommands.clearAllTrustedDomains();
			ToastCommands.createToast({type: 'success', children: i18n._(TRUSTED_DOMAINS_CLEARED_DESCRIPTOR)});
		} catch (error) {
			logger.error('Failed to clear trusted domains', error);
			showChannelErrorModalAfterCurrentModal({
				title: i18n._(FAILED_TO_CLEAR_TRUSTED_DOMAINS_DESCRIPTOR),
				message: i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
				dataFlx:
					'channel.channel-header-components.developer-tools-context-menu.clear-trusted-domains-failed.generic-error-modal',
			});
		} finally {
			setIsClearingTrustedDomains(false);
		}
	}, [i18n]);
	const handleClearTrustedDomainsClick = useCallback(() => {
		openDangerConfirmation({
			onClose,
			title: i18n._(CLEAR_TRUSTED_DOMAINS_DESCRIPTOR),
			description: i18n._(THIS_REMOVES_ALL_TRUSTED_DOMAINS_YOU_WILL_SEE_DESCRIPTOR),
			primaryText: i18n._(CLEAR_TRUSTED_DOMAINS_DESCRIPTOR),
			onPrimary: handleClearTrustedDomains,
		});
	}, [handleClearTrustedDomains, onClose, i18n]);
	const handleResetPremiumState = useCallback(async () => {
		setIsResettingPremiumState(true);
		try {
			await UserCommands.resetPremiumState();
			resetPremiumStateOverrides();
			ToastCommands.createToast({type: 'success', children: i18n._(PREMIUM_STATE_RESET_ON_THE_BACKEND_DESCRIPTOR)});
		} catch (error) {
			logger.error('Failed to reset premium state', error);
			showChannelErrorModalAfterCurrentModal({
				title: i18n._(FAILED_TO_RESET_PREMIUM_STATE_DESCRIPTOR),
				message: i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
				dataFlx:
					'channel.channel-header-components.developer-tools-context-menu.reset-premium-state-failed.generic-error-modal',
			});
		} finally {
			setIsResettingPremiumState(false);
		}
	}, [i18n]);
	const handleResetPremiumStateClick = useCallback(() => {
		openDangerConfirmation({
			onClose,
			title: i18n._(RESET_PREMIUM_STATE_DESCRIPTOR),
			description: i18n._(THIS_CLEARS_YOUR_BACKEND_PREMIUM_STATE_ENTIRELY_INCLUDING_DESCRIPTOR),
			primaryText: i18n._(RESET_PREMIUM_STATE_DESCRIPTOR),
			onPrimary: handleResetPremiumState,
		});
	}, [handleResetPremiumState, onClose, i18n]);
	const clearTrustedDomainsLabel = TrustedDomain.trustAllDomains
		? i18n._(CLEAR_TRUST_ALL_SETTING_DESCRIPTOR)
		: trustedDomainsCount === 0
			? i18n._(NO_TRUSTED_DOMAINS_DESCRIPTOR)
			: i18n._(CLEAR_TRUSTED_DOMAINS_COUNT_DESCRIPTOR, {trustedDomainsCount});
	return (
		<MenuItemSubmenu
			label={i18n._(RUNTIME_UTILITIES_DESCRIPTOR)}
			render={() => (
				<>
					<MenuItem
						icon={
							<PlugIcon
								size={remFromPx(16)}
								weight="bold"
								data-flx="channel.channel-header-components.developer-tools-context-menu.tools-menu.plug-icon"
							/>
						}
						disabled={!socket}
						onClick={() => socket?.reset()}
						data-flx="channel.channel-header-components.developer-tools-context-menu.tools-menu.menu-item.reset"
					>
						<Trans>Reset socket</Trans>
					</MenuItem>
					<MenuItem
						icon={
							<PlugIcon
								size={remFromPx(16)}
								weight="bold"
								data-flx="channel.channel-header-components.developer-tools-context-menu.tools-menu.plug-icon--2"
							/>
						}
						disabled={!socket}
						onClick={() => socket?.simulateNetworkDisconnect()}
						data-flx="channel.channel-header-components.developer-tools-context-menu.tools-menu.menu-item.simulate-network-disconnect"
					>
						<Trans>Disconnect socket</Trans>
					</MenuItem>
					<MenuItem
						icon={
							<PhoneIcon
								size={remFromPx(16)}
								weight="fill"
								data-flx="channel.channel-header-components.developer-tools-context-menu.tools-menu.phone-icon"
							/>
						}
						onClick={() => {
							void MediaEngine.moveToAfkChannel();
						}}
						data-flx="channel.channel-header-components.developer-tools-context-menu.tools-menu.menu-item"
					>
						<Trans>Force move to AFK channel</Trans>
					</MenuItem>
					<MenuItem
						onClick={() => NewDeviceMonitoring.showTestModal()}
						data-flx="channel.channel-header-components.developer-tools-context-menu.tools-menu.menu-item.show-test-modal"
					>
						<Trans>Show new device modal</Trans>
					</MenuItem>
					<MenuItem
						onClick={handleOpenCaptchaModal}
						data-flx="channel.channel-header-components.developer-tools-context-menu.tools-menu.menu-item.open-captcha-modal"
					>
						<Trans>Open captcha modal</Trans>
					</MenuItem>
					<MenuItem
						icon={
							<KeyboardIcon
								size={remFromPx(16)}
								weight="fill"
								data-flx="channel.channel-header-components.developer-tools-context-menu.tools-menu.keyboard-icon"
							/>
						}
						onClick={() => {
							ModalCommands.pushAfterBottomSheetClose(
								onClose,
								ModalCommands.modal(() => (
									<KeyboardModeIntroModal data-flx="channel.channel-header-components.developer-tools-context-menu.tools-menu.keyboard-mode-intro-modal" />
								)),
							);
						}}
						data-flx="channel.channel-header-components.developer-tools-context-menu.tools-menu.menu-item.close"
					>
						<Trans>Show keyboard mode intro</Trans>
					</MenuItem>
					<MenuItem
						onClick={() => openClaimAccountModal({force: true})}
						data-flx="channel.channel-header-components.developer-tools-context-menu.tools-menu.menu-item.open-claim-account-modal"
					>
						<Trans>Open claim account modal</Trans>
					</MenuItem>
					<MenuItem
						onClick={() => {
							if (SudoPrompt.isOpen) return;
							onClose();
							void SudoPrompt.requestVerification({
								method: 'POST',
								path: '<developer-tools>',
							}).catch(() => {});
						}}
						data-flx="channel.channel-header-components.developer-tools-context-menu.tools-menu.menu-item.close--2"
					>
						<Trans>Open sudo verification modal</Trans>
					</MenuItem>
					<MenuItem
						icon={
							<RobotIcon
								size={remFromPx(16)}
								weight="fill"
								data-flx="channel.channel-header-components.developer-tools-context-menu.tools-menu.robot-icon"
							/>
						}
						onClick={() => {
							void PrivateChannelCommands.openDMChannel(FLUXERBOT_ID);
						}}
						data-flx="channel.channel-header-components.developer-tools-context-menu.tools-menu.menu-item--2"
					>
						<Trans>Open system DM</Trans>
					</MenuItem>
					<PendingMenuItem
						danger
						isPending={isForgettingAuthorizedIps}
						label={i18n._(FORGET_AUTHORIZED_IPS_DESCRIPTOR)}
						pendingLabel={i18n._(FORGETTING_AUTHORIZED_IPS_DESCRIPTOR)}
						onClick={handleForgetAuthorizedIpsClick}
						data-flx="channel.channel-header-components.developer-tools-context-menu.tools-menu.pending-menu-item.forget-authorized-ips-click"
					/>
					<PendingMenuItem
						danger
						disabled={trustedDomainsCount === 0 && !TrustedDomain.trustAllDomains}
						isPending={isClearingTrustedDomains}
						label={clearTrustedDomainsLabel}
						pendingLabel={i18n._(CLEARING_TRUSTED_DOMAINS_DESCRIPTOR)}
						onClick={handleClearTrustedDomainsClick}
						data-flx="channel.channel-header-components.developer-tools-context-menu.tools-menu.pending-menu-item.clear-trusted-domains-click"
					/>
					<PendingMenuItem
						danger
						isPending={isResettingPremiumState}
						label={i18n._(RESET_PREMIUM_STATE_DESCRIPTOR)}
						pendingLabel={i18n._(RESETTING_PREMIUM_STATE_DESCRIPTOR)}
						onClick={handleResetPremiumStateClick}
						data-flx="channel.channel-header-components.developer-tools-context-menu.tools-menu.pending-menu-item.reset-premium-state-click"
					/>
					<MenuItem
						icon={
							<BugIcon
								size={16}
								weight="fill"
								data-flx="channel.channel-header-components.developer-tools-context-menu.tools-menu.bug-icon"
							/>
						}
						danger
						onClick={() => {
							RuntimeCrash.triggerFatalCrash(new Error('Triggered React crash from Developer Tools menu'));
						}}
						data-flx="channel.channel-header-components.developer-tools-context-menu.tools-menu.menu-item.trigger-fatal-crash"
					>
						<Trans>Trigger React crash</Trans>
					</MenuItem>
				</>
			)}
			data-flx="channel.channel-header-components.developer-tools-context-menu.tools-menu.menu-item-submenu"
		/>
	);
});
