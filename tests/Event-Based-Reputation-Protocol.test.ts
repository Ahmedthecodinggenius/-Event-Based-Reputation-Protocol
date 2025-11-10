
import { describe, expect, it } from "vitest";

const accounts = simnet.getAccounts();
const address1 = accounts.get("wallet_1")!;
const address2 = accounts.get("wallet_2")!;

/*
  The test below is an example. To learn more, read the testing documentation here:
  https://docs.hiro.so/stacks/clarinet-js-sdk
*/

describe("example tests", () => {
  it("ensures simnet is well initialised", () => {
    expect(simnet.blockHeight).toBeDefined();
  });

  // it("shows an example", () => {
  //   const { result } = simnet.callReadOnlyFn("counter", "get-counter", [], address1);
  //   expect(result).toBeUint(0);
  // });
});

describe("Event-Based-Reputation-Protocol tests", () => {
  it("should allow organizer to update event details", () => {
    const { result: createResult } = simnet.callPublicFn("Event-Based-Reputation-Protocol", "create-event", [
      "Test Event",
      "Test Description",
      1000,
      10,
      1
    ], address1);
    expect(createResult).toBeOk(1);

    const { result: updateResult } = simnet.callPublicFn("Event-Based-Reputation-Protocol", "update-event-details", [
      1,
      "Updated Event",
      "Updated Description",
      2000
    ], address1);
    expect(updateResult).toBeOk(true);

    const { result: eventResult } = simnet.callReadOnlyFn("Event-Based-Reputation-Protocol", "get-event", [1], address1);
    expect(eventResult).toBeSome({
      organizer: address1,
      title: "Updated Event",
      description: "Updated Description",
      date: 2000,
      status: "active",
      rating_sum: 0,
      rating_count: 0,
      max_participants: 10,
      registration_count: 0,
      category_id: 1
    });
  });

  it("should not allow non-organizer to update event details", () => {
    const { result: createResult } = simnet.callPublicFn("Event-Based-Reputation-Protocol", "create-event", [
      "Test Event",
      "Test Description",
      1000,
      10,
      1
    ], address1);
    expect(createResult).toBeOk(1);

    const { result: updateResult } = simnet.callPublicFn("Event-Based-Reputation-Protocol", "update-event-details", [
      1,
      "Updated Event",
      "Updated Description",
      2000
    ], address2);
    expect(updateResult).toBeErr(100);
  });

  it("should not allow updating completed event", () => {
    const { result: createResult } = simnet.callPublicFn("Event-Based-Reputation-Protocol", "create-event", [
      "Test Event",
      "Test Description",
      1000,
      10,
      1
    ], address1);
    expect(createResult).toBeOk(1);

    const { result: completeResult } = simnet.callPublicFn("Event-Based-Reputation-Protocol", "complete-event", [1], address1);
    expect(completeResult).toBeOk(true);

    const { result: updateResult } = simnet.callPublicFn("Event-Based-Reputation-Protocol", "update-event-details", [
      1,
      "Updated Event",
      "Updated Description",
      2000
    ], address1);
    expect(updateResult).toBeErr(113);
  });

  it("should not allow updating non-existent event", () => {
    const { result: updateResult } = simnet.callPublicFn("Event-Based-Reputation-Protocol", "update-event-details", [
      999,
      "Updated Event",
      "Updated Description",
      2000
    ], address1);
    expect(updateResult).toBeErr(102);
  });
});
