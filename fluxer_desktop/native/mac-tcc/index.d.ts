// SPDX-License-Identifier: AGPL-3.0-or-later

export type TccStatus = 'granted' | 'denied' | 'not-determined';

export declare function screenRecordingStatus(): TccStatus;

export declare function requestScreenRecording(): TccStatus;

export declare function inputMonitoringStatus(): TccStatus;

export declare function requestInputMonitoring(): TccStatus;

export declare function probeInputMonitoringAccess(): TccStatus;

export declare const loadError: Error | null;
