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

import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {openNativePermissionSettings} from '~/utils/NativePermissions';
import {isDesktop, isNativeMacOS} from '~/utils/NativeUtils';

export const CameraPermissionDeniedModal = observer(() => {
	const {t} = useLingui();
	if (isDesktop() && isNativeMacOS()) {
		return (
			<ConfirmModal
				title={t`Camera Permission Required`}
				description={t`Fluxer needs access to your camera. Open System Settings → Privacy & Security → Camera, allow Fluxer, and then restart the app.`}
				primaryText={t`Open Settings`}
				primaryVariant="primary"
				onPrimary={() => openNativePermissionSettings('camera')}
				secondaryText={t`Close`}
			/>
		);
	}

	const message = isDesktop()
		? t`Fluxer needs access to your camera. Allow camera access in your operating system privacy settings and restart the app.`
		: t`Fluxer needs access to your camera to enable video chat. Please grant camera permission in your browser settings and try again.`;

	return (
		<ConfirmModal
			title={t`Camera Permission Required`}
			description={message}
			primaryText={t`Understood`}
			onPrimary={() => {}}
		/>
	);
});
