// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {AVIF_FORMAT_LABEL} from '@app/features/app/config/I18nDisplayConstants';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans} from '@lingui/react/macro';

const ANIMATED_NOT_SUPPORTED_DESCRIPTOR = msg({
	message: 'Animated {avifFormatLabel} not supported',
	comment: 'Modal title shown when an animated AVIF upload is rejected by the client.',
});

interface ShowAnimatedAvifUnsupportedModalOptions {
	i18n: I18n;
}

export function showAnimatedAvifUnsupportedModal({i18n}: ShowAnimatedAvifUnsupportedModalOptions): void {
	ModalCommands.push(
		modal(() => (
			<GenericErrorModal
				title={i18n._(ANIMATED_NOT_SUPPORTED_DESCRIPTOR, {avifFormatLabel: AVIF_FORMAT_LABEL})}
				message={
					<Trans>
						Animated {AVIF_FORMAT_LABEL} files aren't supported. Upload a static {AVIF_FORMAT_LABEL} file instead.
					</Trans>
				}
				data-flx="expressions.animated-avif-modal-utils.show-animated-avif-unsupported-modal.confirm-modal"
			/>
		)),
	);
}
