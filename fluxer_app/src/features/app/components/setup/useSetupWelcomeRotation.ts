// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createRandomWelcomeRotationState,
	createWelcomeRotationState,
	WELCOME_ROTATION,
	type WelcomeRotationEntry,
	type WelcomeRotationState,
} from '@app/features/app/components/setup/SetupWizardWelcomeRotation';
import {useEffect, useState} from 'react';

const WELCOME_STEP_MS = 1800;

function advanceWelcomeRotation(rotation: WelcomeRotationState): WelcomeRotationState {
	if (rotation.position + 1 < rotation.order.length) {
		return {...rotation, position: rotation.position + 1};
	}
	return createRandomWelcomeRotationState(rotation.order[rotation.position]);
}

interface WelcomeRotationSchedule {
	readonly current: WelcomeRotationState;
	readonly upcoming: WelcomeRotationState;
}

export interface SetupWelcomeRotation {
	readonly entry: WelcomeRotationEntry;
	readonly upcoming: WelcomeRotationEntry;
}

function scheduleFrom(current: WelcomeRotationState): WelcomeRotationSchedule {
	return {current, upcoming: advanceWelcomeRotation(current)};
}

function advanceWelcomeSchedule(schedule: WelcomeRotationSchedule): WelcomeRotationSchedule {
	return scheduleFrom(schedule.upcoming);
}

function rotationEntry(rotation: WelcomeRotationState): WelcomeRotationEntry {
	return WELCOME_ROTATION[rotation.order[rotation.position] ?? 0] ?? WELCOME_ROTATION[0];
}

export function useSetupWelcomeRotation(localeCode: string): SetupWelcomeRotation {
	const [schedule, setSchedule] = useState<WelcomeRotationSchedule>(() =>
		scheduleFrom(createWelcomeRotationState(localeCode)),
	);
	useEffect(() => {
		setSchedule(scheduleFrom(createWelcomeRotationState(localeCode)));
	}, [localeCode]);
	useEffect(() => {
		const interval = window.setInterval(() => setSchedule(advanceWelcomeSchedule), WELCOME_STEP_MS);
		return () => window.clearInterval(interval);
	}, []);
	return {entry: rotationEntry(schedule.current), upcoming: rotationEntry(schedule.upcoming)};
}
