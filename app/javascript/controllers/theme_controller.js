import { Controller } from "stimulus";

export default class extends Controller {
  static targets = ["gallery"];

  onLangChange(event) {
    console.log("something changed");
    console.log("this.galleryTarget is: ", this.galleryTarget);
  }
}
