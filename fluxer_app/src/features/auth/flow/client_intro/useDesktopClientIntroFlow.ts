// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	advanceDesktopClientIntro,
	readDesktopClientIntroConditions,
} from '@app/features/auth/flow/client_intro/ClientIntroPreferences';
import {
	createDesktopClientIntroSnapshot,
	type DesktopClientIntroConditions,
	type DesktopClientIntroMachineEvent,
	type DesktopClientIntroModel,
	selectDesktopClientIntroModel,
	transitionDesktopClientIntroSnapshot,
} from '@app/features/auth/flow/client_intro/DesktopClientIntroStateMachine';
import MacPermissions from '@app/features/permissions/system/state/MacPermissions';
import {useCallback, useEffect, useState} from 'react';

interface DesktopClientIntroFlowOptions {
	readonly skipWelcome?: boolean;
}

interface DesktopClientIntroFlow extends DesktopClientIntroModel {
	continueFromWelcome: () => void;
	completePreferences: () => void;
	advancePermission: () => void;
	skipPermission: () => void;
}

function readConditions(enabled: boolean, skipWelcome: boolean): DesktopClientIntroConditions {
	const conditions = readDesktopClientIntroConditions(enabled);
	return skipWelcome ? {...conditions, welcomeSeen: true} : conditions;
}

export function useDesktopClientIntroFlow(
	enabled: boolean,
	{skipWelcome = false}: DesktopClientIntroFlowOptions = {},
): DesktopClientIntroFlow {
	const [snapshot, setSnapshot] = useState(() =>
		createDesktopClientIntroSnapshot(readConditions(enabled, skipWelcome), MacPermissions.settledKinds),
	);
	const permissionsHydrated = MacPermissions.isHydrated;
	useEffect(() => {
		const conditions = readConditions(enabled, skipWelcome);
		const settledPermissions = permissionsHydrated ? MacPermissions.settledKinds : [];
		setSnapshot((current) =>
			transitionDesktopClientIntroSnapshot(current, {type: 'intro.sync', ...conditions, settledPermissions}),
		);
	}, [enabled, skipWelcome, permissionsHydrated]);
	const model = selectDesktopClientIntroModel(snapshot);
	const send = useCallback(
		(event: DesktopClientIntroMachineEvent) => setSnapshot(advanceDesktopClientIntro(snapshot, event)),
		[snapshot],
	);
	const continueFromWelcome = useCallback(() => send({type: 'intro.continueWelcome'}), [send]);
	const completePreferences = useCallback(
		() => send({type: 'intro.completePreferences', settledPermissions: MacPermissions.settledKinds}),
		[send],
	);
	const advancePermission = useCallback(
		() => send({type: 'intro.advancePermission', settledPermissions: MacPermissions.settledKinds}),
		[send],
	);
	const skipPermission = useCallback(
		() => send({type: 'intro.skipPermission', settledPermissions: MacPermissions.settledKinds}),
		[send],
	);
	return {
		...model,
		continueFromWelcome,
		completePreferences,
		advancePermission,
		skipPermission,
	};
}
