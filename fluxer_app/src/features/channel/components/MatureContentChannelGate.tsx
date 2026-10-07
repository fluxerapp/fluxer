// SPDX-License-Identifier: AGPL-3.0-or-later

import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import {HelpCenterArticleSlug} from '@app/features/app/config/HelpCenterConstants';
import {MatureContentCheckModal} from '@app/features/auth/components/modals/MatureContentCheckModal';
import styles from '@app/features/channel/components/MatureContentChannelGate.module.css';
import * as GuildMatureContentCommands from '@app/features/guild/commands/GuildMatureContentCommands';
import GuildMatureContentAgree, {
	type AgreementScope,
	MatureContentGateReason,
} from '@app/features/guild/state/GuildMatureContentAgree';
import {
	COMPLETE_MATURE_CONTENT_CHECK_DESCRIPTOR,
	MATURE_CONTENT_DESCRIPTOR,
	PROCEED_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {getDefaultContentWarningText} from '@app/features/messaging/utils/ContentWarningUtils';
import {
	getEffectiveMatureContentGeoContext,
	isMatureContentCheckAvailableInRegion,
} from '@app/features/moderation/utils/MatureContentGeoUtils';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as HelpCenterUtils from '@app/features/ui/utils/HelpCenterUtils';
import {getRegionDisplayName} from '@app/features/user/utils/UserGeo';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

const MATURE_COMMUNITY_NOT_AVAILABLE_DESCRIPTOR = msg({
	message: 'This mature community is not available to your account.',
	comment: 'Body text on the mature content gate when the viewer cannot open a mature community.',
});
const MATURE_CATEGORY_NOT_AVAILABLE_DESCRIPTOR = msg({
	message: 'This mature category is not available to your account.',
	comment: 'Body text on the mature content gate when the viewer cannot open a mature category.',
});
const MATURE_CHANNEL_NOT_AVAILABLE_DESCRIPTOR = msg({
	message: 'This mature channel is not available to your account.',
	comment: 'Body text on the mature content gate when the viewer cannot open a mature channel.',
});

interface Props {
	channelId?: string | null;
	guildId?: string | null;
	reason: MatureContentGateReason;
	scope?: AgreementScope;
}

export const MatureContentChannelGate = observer(({channelId, guildId, reason, scope: scopeOverride}: Props) => {
	const {i18n} = useLingui();
	const learnMoreUrl = HelpCenterUtils.getURL(HelpCenterArticleSlug.ChangeDateOfBirth);
	const resolved = GuildMatureContentAgree.getResolvedContext({channelId: channelId ?? null, guildId});
	const scope: AgreementScope = scopeOverride ?? resolved.scope;
	const effectiveMatureContent = resolved.effectiveMatureContent;
	const hasCustomWarningText = resolved.effectiveWarningText != null && resolved.effectiveWarningText.length > 0;
	const warningBody = hasCustomWarningText ? resolved.effectiveWarningText : getDefaultContentWarningText(i18n);
	const handleOpenMatureContentCheck = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<MatureContentCheckModal data-flx="channel.mature-content-channel-gate.handle-open-mature-content-check.mature-content-check-modal" />
			)),
		);
	}, []);
	const handleProceed = () => {
		if (scope === 'guild' && resolved.guildId) {
			GuildMatureContentCommands.agreeToGuild(resolved.guildId);
			return;
		}
		if (scope === 'category' && (resolved.scopeId || resolved.categoryId)) {
			GuildMatureContentCommands.agreeToCategory((resolved.scopeId ?? resolved.categoryId) as string);
			return;
		}
		if (resolved.scopeId) {
			GuildMatureContentCommands.agreeToChannel(resolved.scopeId);
			return;
		}
		if (channelId) {
			GuildMatureContentCommands.agreeToChannel(channelId);
		}
	};
	const renderContent = () => {
		switch (reason) {
			case MatureContentGateReason.GEO_RESTRICTED: {
				const {countryCode, regionCode} = getEffectiveMatureContentGeoContext();
				const regionName = getRegionDisplayName(i18n, countryCode ?? undefined, regionCode ?? undefined);
				const matureContentCheckAvailable = isMatureContentCheckAvailableInRegion();
				return (
					<>
						<h2 className={styles.title} data-flx="channel.mature-content-channel-gate.render-content.title">
							{i18n._(MATURE_CONTENT_DESCRIPTOR)}
						</h2>
						{matureContentCheckAvailable ? (
							<p
								className={styles.description}
								data-flx="channel.mature-content-channel-gate.render-content.description"
							>
								<Trans>
									Due to mature content laws in {regionName}, mature content is blocked until you complete the mature
									content check.
								</Trans>
							</p>
						) : (
							<p
								className={styles.description}
								data-flx="channel.mature-content-channel-gate.render-content.description--2"
							>
								<Trans>
									Due to mature content laws in {regionName}, mature content is not available from here. Mature content
									checks are available only in the UK.
								</Trans>
							</p>
						)}
						{matureContentCheckAvailable && (
							<Button
								onClick={handleOpenMatureContentCheck}
								variant="primary"
								data-flx="channel.mature-content-channel-gate.render-content.button.open-mature-content-check"
							>
								{i18n._(COMPLETE_MATURE_CONTENT_CHECK_DESCRIPTOR)}
							</Button>
						)}
					</>
				);
			}
			case MatureContentGateReason.MATURE_CONTENT_CHECK_REQUIRED:
				if (scope === 'guild') {
					return (
						<>
							<h2 className={styles.title} data-flx="channel.mature-content-channel-gate.render-content.title--2">
								<Trans>Mature community</Trans>
							</h2>
							<p
								className={styles.description}
								data-flx="channel.mature-content-channel-gate.render-content.description--3"
							>
								{learnMoreUrl ? (
									<Trans>
										This mature community is not available to your account.{' '}
										<ExternalLink
											href={learnMoreUrl}
											data-flx="channel.mature-content-channel-gate.render-content.external-link"
										>
											Learn more
										</ExternalLink>
									</Trans>
								) : (
									i18n._(MATURE_COMMUNITY_NOT_AVAILABLE_DESCRIPTOR)
								)}
							</p>
						</>
					);
				}
				if (scope === 'category') {
					return (
						<>
							<h2 className={styles.title} data-flx="channel.mature-content-channel-gate.render-content.title--3">
								<Trans>Mature category</Trans>
							</h2>
							<p
								className={styles.description}
								data-flx="channel.mature-content-channel-gate.render-content.description--4"
							>
								{learnMoreUrl ? (
									<Trans>
										This mature category is not available to your account.{' '}
										<ExternalLink
											href={learnMoreUrl}
											data-flx="channel.mature-content-channel-gate.render-content.external-link--2"
										>
											Learn more
										</ExternalLink>
									</Trans>
								) : (
									i18n._(MATURE_CATEGORY_NOT_AVAILABLE_DESCRIPTOR)
								)}
							</p>
						</>
					);
				}
				return (
					<>
						<h2 className={styles.title} data-flx="channel.mature-content-channel-gate.render-content.title--4">
							<Trans>Mature channel</Trans>
						</h2>
						<p
							className={styles.description}
							data-flx="channel.mature-content-channel-gate.render-content.description--5"
						>
							{learnMoreUrl ? (
								<Trans>
									This mature channel is not available to your account.{' '}
									<ExternalLink
										href={learnMoreUrl}
										data-flx="channel.mature-content-channel-gate.render-content.external-link--3"
									>
										Learn more
									</ExternalLink>
								</Trans>
							) : (
								i18n._(MATURE_CHANNEL_NOT_AVAILABLE_DESCRIPTOR)
							)}
						</p>
					</>
				);
			default: {
				if (!effectiveMatureContent) {
					const title =
						scope === 'guild' ? (
							<Trans>Community content warning</Trans>
						) : scope === 'category' ? (
							<Trans>Category content warning</Trans>
						) : (
							<Trans>Channel content warning</Trans>
						);
					return (
						<>
							<h2 className={styles.title} data-flx="channel.mature-content-channel-gate.render-content.title--5">
								{title}
							</h2>
							<p
								className={styles.description}
								data-flx="channel.mature-content-channel-gate.render-content.description--6"
							>
								{warningBody}
							</p>
							<Button
								onClick={handleProceed}
								variant="primary"
								data-flx="channel.mature-content-channel-gate.render-content.button.proceed"
							>
								<Trans>I understand</Trans>
							</Button>
						</>
					);
				}
				if (scope === 'guild') {
					return (
						<>
							<h2 className={styles.title} data-flx="channel.mature-content-channel-gate.render-content.title--6">
								{i18n._(MATURE_CONTENT_DESCRIPTOR)}
							</h2>
							<p
								className={styles.description}
								data-flx="channel.mature-content-channel-gate.render-content.description--7"
							>
								{hasCustomWarningText ? (
									warningBody
								) : (
									<Trans>
										This community is marked for mature content and may contain material that may be inappropriate for
										some users.
									</Trans>
								)}
							</p>
							<Button
								onClick={handleProceed}
								variant="danger"
								data-flx="channel.mature-content-channel-gate.render-content.button.proceed--2"
							>
								{i18n._(PROCEED_DESCRIPTOR)}
							</Button>
						</>
					);
				}
				if (scope === 'category') {
					return (
						<>
							<h2 className={styles.title} data-flx="channel.mature-content-channel-gate.render-content.title--7">
								{i18n._(MATURE_CONTENT_DESCRIPTOR)}
							</h2>
							<p
								className={styles.description}
								data-flx="channel.mature-content-channel-gate.render-content.description--8"
							>
								{hasCustomWarningText ? (
									warningBody
								) : (
									<Trans>
										This category is marked for mature content and may contain material that may be inappropriate for
										some users.
									</Trans>
								)}
							</p>
							<Button
								onClick={handleProceed}
								variant="danger"
								data-flx="channel.mature-content-channel-gate.render-content.button.proceed--3"
							>
								{i18n._(PROCEED_DESCRIPTOR)}
							</Button>
						</>
					);
				}
				return (
					<>
						<h2 className={styles.title} data-flx="channel.mature-content-channel-gate.render-content.title--8">
							{i18n._(MATURE_CONTENT_DESCRIPTOR)}
						</h2>
						<p
							className={styles.description}
							data-flx="channel.mature-content-channel-gate.render-content.description--9"
						>
							{hasCustomWarningText ? (
								warningBody
							) : (
								<Trans>
									This channel is marked for mature content and may contain material that may be inappropriate for some
									users.
								</Trans>
							)}
						</p>
						<Button
							onClick={handleProceed}
							variant="danger"
							data-flx="channel.mature-content-channel-gate.render-content.button.proceed--4"
						>
							{i18n._(PROCEED_DESCRIPTOR)}
						</Button>
					</>
				);
			}
		}
	};
	return (
		<div className={styles.container} data-flx="channel.mature-content-channel-gate.container">
			<div className={styles.content} data-flx="channel.mature-content-channel-gate.content">
				{renderContent()}
			</div>
		</div>
	);
});
