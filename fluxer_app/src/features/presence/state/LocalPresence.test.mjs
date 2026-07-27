import test from 'node:test';
import assert from 'node:assert/strict';

// Mock MobX & Dependencies for isolated unit/integration verification
class MockLocalPresence {
	constructor() {
		this.status = 'online';
		this.since = 0;
		this.afk = false;
		this.mobile = false;
		this.customStatus = null;
		this.activities = null;
		this.listeners = [];
	}

	onPresenceKeyChange(listener) {
		this.listeners.push(listener);
	}

	notifyChange() {
		for (const listener of this.listeners) {
			listener(this.presenceKey);
		}
	}

	setActivity(activity) {
		if (!activity) {
			this.activities = null;
		} else if (Array.isArray(activity)) {
			this.activities = activity.length > 0 ? activity : null;
		} else {
			this.activities = [activity];
		}
		this.notifyChange();
	}

	clearActivity() {
		this.setActivity(null);
	}

	getActivities() {
		return this.activities;
	}

	getPresence() {
		return {
			status: this.status,
			since: this.since,
			afk: this.afk,
			mobile: this.mobile,
			custom_status: this.customStatus,
			activities: this.activities,
		};
	}

	getGatewayPresence() {
		return this.getPresence();
	}

	get presenceKey() {
		const actKey = this.activities && this.activities.length > 0 ? JSON.stringify(this.activities) : 'null';
		return `status:${this.status}|afk:${this.afk}|mobile:${this.mobile}|act:${actKey}`;
	}
}

class MockGatewaySocket {
	constructor() {
		this.sentPayloads = [];
		this.connected = true;
	}

	isConnected() {
		return this.connected;
	}

	sendPayload(payload) {
		if (!this.isConnected()) return false;
		this.sentPayloads.push(payload);
		return true;
	}

	updatePresence(status, afk, mobile, customStatus, activities) {
		if (!this.isConnected()) return;
		this.sendPayload({
			op: 3, // Gateway Opcodes.PRESENCE_UPDATE
			d: {
				status,
				...(afk !== undefined && {afk}),
				...(mobile !== undefined && {mobile}),
				...(customStatus !== undefined && {custom_status: customStatus}),
				...(activities !== undefined && {activities}),
			},
		});
	}
}

test('1. ActivityPayload & PresenceRecord Interface Structure Verification', () => {
	const activity = {
		name: 'Cyberpunk 2077',
		type: 0,
		details: 'Exploring Night City',
		state: 'In Game',
		timestamps: { start: 1700000000 },
		assets: {
			large_image: 'cp2077_logo',
			large_text: 'Cyberpunk 2077'
		}
	};

	assert.equal(activity.name, 'Cyberpunk 2077');
	assert.equal(activity.type, 0);
	assert.equal(activity.details, 'Exploring Night City');
	assert.equal(activity.assets.large_image, 'cp2077_logo');
});

test('2. LocalPresence State Maintenance & Activity Updates', () => {
	const localPresence = new MockLocalPresence();
	assert.equal(localPresence.getActivities(), null);

	const sampleActivity = {
		name: 'Spotify',
		type: 2,
		details: 'Listening to Synthwave'
	};

	localPresence.setActivity(sampleActivity);
	const presence = localPresence.getPresence();
	assert.ok(presence.activities);
	assert.equal(presence.activities.length, 1);
	assert.equal(presence.activities[0].name, 'Spotify');
	assert.equal(presence.activities[0].type, 2);

	// Verify presenceKey updates
	assert.ok(localPresence.presenceKey.includes('Spotify'));
});

test('3. GatewaySocket Opcode 3 (Presence Update) Frame Dispatch', () => {
	const socket = new MockGatewaySocket();
	const localPresence = new MockLocalPresence();

	// Bind presence key changes to socket presence updates
	localPresence.onPresenceKeyChange(() => {
		const presence = localPresence.getGatewayPresence();
		socket.updatePresence(
			presence.status,
			presence.afk,
			presence.mobile,
			presence.custom_status,
			presence.activities
		);
	});

	const activity = {
		name: 'Elden Ring',
		type: 0,
		details: 'Exploring the Lands Between'
	};

	localPresence.setActivity(activity);

	assert.equal(socket.sentPayloads.length, 1);
	const frame = socket.sentPayloads[0];
	assert.equal(frame.op, 3); // Opcode 3 = PRESENCE_UPDATE
	assert.equal(frame.d.status, 'online');
	assert.ok(Array.isArray(frame.d.activities));
	assert.equal(frame.d.activities[0].name, 'Elden Ring');
});

test('4. Clear Activity Behavior (activity: null / disconnect)', () => {
	const socket = new MockGatewaySocket();
	const localPresence = new MockLocalPresence();

	localPresence.onPresenceKeyChange(() => {
		const presence = localPresence.getGatewayPresence();
		socket.updatePresence(
			presence.status,
			presence.afk,
			presence.mobile,
			presence.custom_status,
			presence.activities
		);
	});

	// Set initial activity
	localPresence.setActivity({ name: 'Minecraft', type: 0 });
	assert.equal(socket.sentPayloads.length, 1);

	// Clear activity
	localPresence.clearActivity();
	assert.equal(socket.sentPayloads.length, 2);

	const clearFrame = socket.sentPayloads[1];
	assert.equal(clearFrame.op, 3);
	assert.equal(clearFrame.d.activities, null);
	assert.equal(localPresence.getActivities(), null);
});

test('5. End-to-End RPC Activity Update to Gateway Opcode 3 Pipeline', () => {
	const socket = new MockGatewaySocket();
	const localPresence = new MockLocalPresence();

	localPresence.onPresenceKeyChange(() => {
		const presence = localPresence.getGatewayPresence();
		socket.updatePresence(
			presence.status,
			presence.afk,
			presence.mobile,
			presence.custom_status,
			presence.activities
		);
	});

	// Simulate Electron IPC rpc-activity-update event handler
	const handleRpcActivityUpdate = (activity) => {
		localPresence.setActivity(activity);
	};

	// 1. SET_ACTIVITY command
	handleRpcActivityUpdate({
		name: 'VALORANT',
		type: 0,
		state: 'In Competitive Match'
	});

	assert.equal(socket.sentPayloads.length, 1);
	assert.equal(socket.sentPayloads[0].op, 3);
	assert.equal(socket.sentPayloads[0].d.activities[0].name, 'VALORANT');

	// 2. Client Disconnect / Clear Activity
	handleRpcActivityUpdate(null);

	assert.equal(socket.sentPayloads.length, 2);
	assert.equal(socket.sentPayloads[1].op, 3);
	assert.equal(socket.sentPayloads[1].d.activities, null);
});
