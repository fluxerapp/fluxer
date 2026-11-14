// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import {HelpCenterArticleSlug} from '@app/features/app/config/HelpCenterConstants';
import {MatureContentGateReason} from '@app/features/guild/state/GuildMatureContentAgree';
import {getEffectiveMatureContentGeoContext} from '@app/features/moderation/utils/MatureContentGeoUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as HelpCenterUtils from '@app/features/ui/utils/HelpCenterUtils';
import {getRegionDisplayName} from '@app/features/user/utils/UserGeo';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

const MATURE_MEDIA_DESCRIPTOR = msg({
	message: 'Mature media',
	comment: 'Short label in the mature media gate details modal. Keep it concise.',
});

const MATURE_MEDIA_NOT_AVAILABLE_DESCRIPTOR = msg({
	message: 'This mature media is not available to your account.',
	comment: 'Body text in the mature media gate details modal when the viewer cannot open mature media.',
});

interface MatureMediaGateDetailsModalProps {
	reason: MatureContentGateReason;
}

export const MatureMediaGateDetailsModal = observer(({reason}: MatureMediaGateDetailsModalProps) => {
	const {i18n} = useLingui();
	const learnMoreUrl = HelpCenterUtils.getURL(HelpCenterArticleSlug.ChangeDateOfBirth);
	const handleClose = useCallback(() => {
		ModalCommands.pop();
	}, []);
	if (reason === MatureContentGateReason.GEO_RESTRICTED) {
		const {countryCode, regionCode} = getEffectiveMatureContentGeoContext();
		const regionName = getRegionDisplayName(i18n, countryCode ?? undefined, regionCode ?? undefined);
		return (
			<Modal.Root size="small" centered data-flx="moderation.mature-media-gate-details-modal.modal-root">
				<Modal.Header
					title={i18n._(MATURE_MEDIA_DESCRIPTOR)}
					data-flx="moderation.mature-media-gate-details-modal.modal-header"
				/>
				<Modal.Content data-flx="moderation.mature-media-gate-details-modal.modal-content">
					<Modal.ContentLayout data-flx="moderation.mature-media-gate-details-modal.modal-content-layout">
						<Modal.Description data-flx="moderation.mature-media-gate-details-modal.modal-description">
							<Trans>
								Due to mature content laws in {regionName}, this media is not available from your current region.
							</Trans>
						</Modal.Description>
					</Modal.ContentLayout>
				</Modal.Content>
				<Modal.DismissFooter
					onDismiss={handleClose}
					data-flx="moderation.mature-media-gate-details-modal.dismiss-footer"
				/>
			</Modal.Root>
		);
	}
	return (
		<Modal.Root size="small" centered data-flx="moderation.mature-media-gate-details-modal.modal-root--2">
			<Modal.Header
				title={i18n._(MATURE_MEDIA_DESCRIPTOR)}
				data-flx="moderation.mature-media-gate-details-modal.modal-header--2"
			/>
			<Modal.Content data-flx="moderation.mature-media-gate-details-modal.modal-content--2">
				<Modal.ContentLayout data-flx="moderation.mature-media-gate-details-modal.modal-content-layout--2">
					<Modal.Description data-flx="moderation.mature-media-gate-details-modal.modal-description--4">
						{learnMoreUrl ? (
							<Trans>
								This mature media is not available to your account.{' '}
								<ExternalLink href={learnMoreUrl} data-flx="moderation.mature-media-gate-details-modal.external-link">
									Learn more
								</ExternalLink>
							</Trans>
						) : (
							i18n._(MATURE_MEDIA_NOT_AVAILABLE_DESCRIPTOR)
						)}
					</Modal.Description>
				</Modal.ContentLayout>
			</Modal.Content>
		</Modal.Root>
	);
});
